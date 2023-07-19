-module(aeplugin_scm_app).
-behavior(application).

-export([ start/2,
          start_phase/3,
          stop/1 ]).

-export([ check_env/0 ]).

-export([ info/0 ]).

-define(PLUGIN_NAME_STR, <<"aeplugin_scm">>).
-define(SCHEMA_FNAME, "aeplugin_scm_config_schema.json").
-define(OS_ENV_PFX, "AESCM").
-define(LAGER_SINK, ae_scm_lager_event).
-define(LAGER_CHAT_SINK, ae_scm_chat_lager_event).

start(_Type, _Args) ->
    create_lager_sinks(),
    {ok, Pid} = aeplugin_scm_sup:start_link(),
    ok = start_http_api(),
    {ok, Pid}.

start_phase(check_config, _Type, _Args) ->
    case aeu_plugins:check_config(?PLUGIN_NAME_STR, ?SCHEMA_FNAME, ?OS_ENV_PFX) of
        Config when is_map(Config) ->
            apply_config(Config);
        not_found ->
            ok
    end.

stop(_State) ->
    stop_http_api(),
    ok.

check_env() ->
    ok.

create_lager_sinks() ->
    create_lager_sink(?LAGER_SINK),
    create_lager_sink(?LAGER_CHAT_SINK).

create_lager_sink(Sink) ->
    case lists:member(Sink, lager:list_all_sinks()) of
        true ->
            ok;
        false ->
            lager_app:configure_sink(
              Sink, [{handlers, sink_handlers(Sink)}])
    end.

sink_handlers(?LAGER_CHAT_SINK = Sink) ->
    [{lager_console_backend, [{level, info}]},
     sink_file_backend(Sink)];
sink_handlers(?LAGER_SINK = Sink) ->
    [sink_file_backend(Sink)].

sink_file_backend(Sink) ->
    Fname = sink_file_name(Sink),
    {lager_file_backend,
     [{file, Fname},
      {level, debug},
      {size, 4194303},
      {date, "$D0"},
      {count, 3}
     ]}.

sink_file_name(?LAGER_SINK) -> "ae_scm.log";
sink_file_name(?LAGER_CHAT_SINK) -> "ae_scm_chat.log".

start_http_api() ->
    Port = get_http_api_port(),
    Dispatch = cowboy_router:compile(aeplugin_scm_handler:routes()),
    {ok, _} = cowboy:start_clear(scm_listener,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}),
    ok.

stop_http_api() ->
    cowboy:stop_listener(scm_listener).

get_http_api_port() ->
    list_to_integer(os:getenv("AE_SCM_PORT", "3344")).

apply_config(_Config) ->
    ok.

info() ->
    #{
     }.
