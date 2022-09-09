%%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
-module(aeplugin_scm_server).
-behaviour(gen_server).

-export([ start_link/0 ]).

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

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% TODO: Make configurable, of course
    jobs:add_queue(?MODULE, [ {producer, fun job_init/0}
                            , {standard_counter, 10} ]),
    #{pubkey := Pub} = KP = aecore_env:patron_keypair_for_testing(),
    Id = aeser_id:create(account, Pub),
    aeplugin_scm_registry:register_merchant(Id, [<<"demo_merchant">>], [<<"coffee">>, <<"deli">>]),
    ChOpts = channel_opts(KP),
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

%%
channel_opts(#{pubkey := PubKey} = KeyPair) ->
    Sign = aeplugin_scm_signing:verify_signing_config(KeyPair),
    #{ sign => Sign
     , channel_reserve => 100000000000000
     , initiator => any
     , initiator_amount => 1000000000000000
     , lock_period => 10
     , noise => [{noise,<<"Noise_NN_25519_ChaChaPoly_BLAKE2b">>}]
     , push_amount => 0
     , responder => PubKey
     , responder_amount => 1000000000000000 }.
