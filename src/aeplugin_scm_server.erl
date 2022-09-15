%%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
-module(aeplugin_scm_server).
-behaviour(gen_server).

-export([ start_link/0 ]).

-export([ market_pubkey/0
        , market_endpoint/0 ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).

-define(HOST, {127,0,0,1}).
-define(PORT, 3333).

-record(st, { channel_opts
            , host = ?HOST
            , port = ?PORT
            , sessions = [] }).

%%% Currently, we cheat and use the demo patron keypair as the hard-coded
%%% Market pubkey. TODO: Make configurable

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

market_pubkey() ->
    #{pubkey := Pub} = market_keypair(),
    Pub.

market_endpoint() ->
    {?HOST, ?PORT}.

market_keypair() ->
    %% TODO: make configurable
    aecore_env:patron_keypair_for_testing().

init([]) ->
    %% TODO: Make configurable, of course
    jobs:add_queue(?MODULE, [ {producer, fun job_init/0}
                            , {standard_counter, 10} ]),
    KP = market_keypair(),
    aeplugin_scm_registry:create_chat_group(<<"SCM">>),
    %% Id = aeser_id:create(account, Pub),
    ChOpts = aeplugin_scm_util:channel_opts(responder, KP, any),
    {ok, #st{ channel_opts = ChOpts }}.

handle_call(get_channel_opts, _From, #st{ channel_opts = ChOpts
                                        , host = Host
                                        , port = Port } = St) ->
    %% TODO: perhaps keep a list of pending session reconnects?
    {reply, {Host, Port, ChOpts}, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

job_init() ->
    {Host, Port, Opts} = get_channel_opts(),
    aeplugin_scm_sc:respond(Host, Port, Opts).

get_channel_opts() ->
    gen_server:call(?MODULE, get_channel_opts).
