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

-record(st, { channel_id
            , fsm
            , sign
            , create_tx
            , opts}).

-define(TIMEOUT, 5000).
-define(LONG_TIMEOUT, 60000).

-define(dbg(X), dbg(?LINE, X)).             

initiate(Host, Port, Opts) when is_map(Opts) ->
    start(#{ host => Host
           , port => Port
           , opts => Opts#{role => initiator}}).

respond(Host, Port, Opts) when is_map(Opts) ->
    start(#{ host => Host
           , port => Port
           , opts => Opts#{role => responder}}).

defaults() ->
    #{ lock_period => 10
     , minimum_depth => 6
     , defer_min_depth => true
     }.

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
            Res = case Role of
                      initiator ->
                          aesc_client:initiate(Host, Port, Opts);
                      responder ->
                          %% TODO: Host could in this case refer to a specific interface?
                          aesc_client:respond(Port, Opts)
                  end,
            case Res of
                {ok, Pid} ->
                    #{} = St = init_channel(#{fsm => Pid, opts => Opts, sign => Sign}),
                    {ok, St};
                {error, _} = Error ->
                    Error
            end;
        error ->
            error(no_signing_method)
    end.

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
    after ?TIMEOUT ->
            error({timeout, fsm_up})
    end.

init_channel_initiator(St) ->
    receive_info_report(channel_accept, St),
    SignedTx = await_tx_to_sign(create_tx, St),
    receive_info_report(funding_signed, St),
    #{channel_id := ChId} =
        receive_on_chain_tx(channel_create_tx, funding_signed, St),
    finalize_channel_setup(St#{channel_id => ChId, create_tx => aetx_sign:tx(SignedTx)}).

init_channel_responder(St) ->
    receive_info_report(channel_open, St, ?LONG_TIMEOUT),
    receive_info_report(funding_created, St),
    await_tx_to_sign(funding_created, St),
    #{channel_id := ChId} =
        receive_on_chain_tx(channel_create_tx, funding_created, St),
    finalize_channel_setup(St#st{channel_id = ChId}).

%% Common for both initiator and responder
%%
finalize_channel_setup(St) ->
    receive_on_chain_tx(channel_create_tx, channel_changed, St),
    receive_info_report(own_funding_locked, St),
    receive_info_report(funding_locked, St),
    receive_info_report(open, St),
    St.

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

await_tx_to_sign(Tag, #{fsm := Fsm} = St) ->
    receive
        {aesc_fsm, Fsm, #{ type := sign
                         , tag  := Tag
                         , info := SignedTx }} ->
            NewSignedTx = aeplugin_scm_signing:sign_tx(SignedTx, St),
            Fsm ! {signed, Tag, NewSignedTx},
            NewSignedTx
    after ?TIMEOUT ->
            error({signing_timeout, Tag})
    end.

dbg(_Line, _X) ->
    ok.
