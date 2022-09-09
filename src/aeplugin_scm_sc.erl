%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
-module(aeplugin_scm_sc).
-behavior(gen_server).

-export([ initiate/3
        , respond/3 ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).

-type st() :: #{ channel_id := binary()
               , fsm := pid()
               , sign := aeplugin_scm_signing:signing_fun()
               , create_tx := map()
               , opts := map() }.

-define(TIMEOUT, 5000).
-define(LONG_TIMEOUT, 60000).

-define(dbg(X), dbg(?LINE, X)).             

initiate(Host, Port, Opts) when is_map(Opts) ->
    start(#{ host => Host
           , port => Port
           , opts => apply_defaults(initiator, Opts) }).

respond(Host, Port, Opts) when is_map(Opts) ->
    start(#{ host => Host
           , port => Port
           , opts => apply_defaults(responder, Opts) }).

close_mutual(ChPid) ->
    gen_server:call(ChPid, close_mutual).

apply_defaults(Role, Opts) ->
    Opts0 = maps:merge(defaults(), role_specific_defaults(Role)),
    Opts1 = maps:merge(Opts0, Opts),
    Opts1#{role => Role}.

defaults() ->
    #{ lock_period => 10
     , minimum_depth => 6
     , defer_min_depth => true
     }.

role_specific_defaults(responder) ->
    #{timeouts => #{ accept => infinity
                   , idle   => infinity }};
role_specific_defaults(initiator) ->
    #{}.

start(Opts) ->
    gen_server:start(?MODULE, Opts#{parent => self()}, []).


init(Opts) ->
    start_channel(Opts).


handle_call(_Rec, _From, St) ->
    {reply, {error, unknown_request}, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(_Mst, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

start_channel(#{ host := Host, port := Port, opts := #{role := Role} = Opts0}) ->
    ?dbg(#{opts => Opts0}),
    case maps:take(sign, Opts0) of
        {Sign0, Opts} ->
            Sign = aeplugin_scm_signing:verify_signing_config(Sign0),
            FsmOpts = maps:without(scm_only_opts(), Opts),
            Res = case Role of
                      initiator ->
                          aesc_client:initiate(Host, Port, FsmOpts);
                      responder ->
                          %% TODO: Host could in this case refer to a specific interface?
                          aesc_client:respond(Port, FsmOpts)
                  end,
            case Res of
                {ok, Pid} ->
                    #{} = St = init_channel(#{ fsm => Pid
                                             , opts => Opts
                                             , sign => Sign}),
                    {ok, St};
                {error, _} = Error ->
                    Error
            end;
        error ->
            error(no_signing_method)
    end.

scm_only_opts() ->
    [sign, scm].

-spec init_channel(map()) -> st().
init_channel(#{fsm := Fsm, opts := #{role := Role}} = St0) ->
    receive
        {aesc_fsm, Fsm, #{ type := report
                         , tag  := info
                         , info := {fsm_up, FsmIdWrapper}}} when is_function(FsmIdWrapper) ->
            FsmId = aesc_fsm_id:retrieve(FsmIdWrapper),
            St = St0#{fsm_id => FsmId},
            case Role of
                initiator ->
                    init_channel_initiator(St);
                responder ->
                    init_channel_responder(St)
            end
    after timeout(accept, St0) ->
            error({timeout, fsm_up})
    end.

init_channel_initiator(St) ->
    receive_info_report(channel_accept, St, timeout(accept, St)),
    SignedTx = await_tx_to_sign(create_tx, St, timeout(funding_create, St)),
    receive_info_report(funding_signed, St, timeout(funding_sign, St)),
    #{channel_id := ChId} =
        receive_on_chain_tx(channel_create_tx, funding_signed, St),
    finalize_channel_setup(St#{channel_id => ChId, create_tx => aetx_sign:innermost_tx(SignedTx)}).

init_channel_responder(St) ->
    receive_info_report(channel_open, St, timeout(accept, St)),
    receive_info_report(funding_created, St, timeout(funding_create, St)),
    await_tx_to_sign(funding_created, St, timeout(funding_create, St)),
    #{channel_id := ChId} =
        receive_on_chain_tx(channel_create_tx, funding_created, St),
    finalize_channel_setup(St#{channel_id => ChId}).

%% Common for both initiator and responder
%%
finalize_channel_setup(St0) ->
    #{info := #{tx := SignedTx}} = receive_on_chain_tx(channel_create_tx, channel_changed, St0),
    St = get_pubkeys_from_signed_create_tx(SignedTx, St0),
    receive_info_report(own_funding_locked, St, ?LONG_TIMEOUT),
    lager:info("Awaiting min_depth confirmation (~p blocks)", [get_opt(minimum_depth, St)]),
    receive_info_report(funding_locked, St),
    receive_info_report(open, St),
    aeplugin_scm_registry:register_session(St),
    St.

get_opt(Key, #{opts := Opts}) ->
    maps:get(Key, Opts, undefined).

receive_info_report(Info, St) ->
    receive_info_report(Info, St, ?TIMEOUT).

receive_info_report(Info, #{fsm := Fsm}, Timeout) ->
    receive
        {aesc_fsm, Fsm, #{ type := report
                         , tag  := info
                         , info := Info } = Msg} ->
            Msg
    after Timeout ->
            error({timeout, {info_report, Info}})
    end.

receive_on_chain_tx(TxType, Info, #{fsm := Fsm}) ->
    receive
        {aesc_fsm, Fsm, #{ type := report
                         , tag  := on_chain_tx
                         , info := #{ info := Info
                                    , type := TxType
                                    , tx   := _SignedTx } } = Msg} ->
            Msg
    after ?LONG_TIMEOUT ->
            error({timeout, {on_chain_tx, TxType, Info}})
    end.

await_tx_to_sign(Tag, #{fsm := Fsm} = St, Timeout) ->
    receive
        {aesc_fsm, Fsm, #{ type := sign
                         , tag  := Tag
                         , info := #{signed_tx := SignedTx} }} ->
            NewSignedTx = aeplugin_scm_signing:sign_tx(SignedTx, St),
            aesc_fsm:signing_response(Fsm, Tag, NewSignedTx),
            NewSignedTx
    after Timeout ->
            error({signing_timeout, Tag})
    end.

get_pubkeys_from_signed_create_tx(SignedTx, St) ->
    Tx = aetx_sign:innermost_tx(SignedTx),
    {aesc_create_tx, CTx} = aetx:specialize_callback(Tx),
    IId = aesc_create_tx:initiator_id(CTx),
    RId = aesc_create_tx:responder_id(CTx),
    St#{ids => #{initiator => IId, responder => RId}}.

timeout(Name, #{opts := Opts}) ->
    maps:get(Name, maps:get(timeouts, Opts, #{}), ?TIMEOUT).

dbg(_Line, _X) ->
    ok.
