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

start(_Type, _Args) ->
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

apply_config(Config) ->
    ok.

info() ->
    #{
     }.
