%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
-module(aeplugin_scm_sc).
-behavior(gen_server).

-export([ initiate/3
        , respond/3 ]).

-export([ chat/3
        , group_chat/3
        , send_msg/3
        , send_payment/4
        , get_balances/1
        ]).

-export([ close_mutual/1
        ]).

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

-define(ERR_USER, 128).   % from aesc_codec.hrl
-define(ERR_FORBIDDEN, ?ERR_USER + 1).
-define(ERR_NOT_FOUND, ?ERR_USER + 2).
-define(ERR_UNKNOWN, ?ERR_USER + 3).

%% crypto:strong_rand_bytes(enacl:box_NONCEBYTES())
-define(NONCE, <<171,200,46,25,40,23,177,61,66,168,47,183,
                 226,106,63,8,126,157,76,164,171,243,152,242>>).

-define(dbg(X), dbg(?LINE, X)).

initiate(Host, Port, Opts) when is_map(Opts) ->
    start(#{ host => Host
           , port => Port
           , opts => apply_defaults(initiator, Opts) }).

respond(Host, Port, Opts) when is_map(Opts) ->
    start(#{ host => Host
           , port => Port
           , opts => apply_defaults(responder, Opts) }).

chat(From, To, Msg) ->
    lager:debug("From = ~p, To = ~p, Msg = ~s", [From, To, Msg]),
    send_msg(From, To, jsx:encode(#{<<"chat">> => Msg})).

group_chat(From, Group, Text) ->
    Members = aeplugin_scm_registry:chat_group_members(Group),
    case lists:member(From, Members) of
        true ->
            Msg = jsx:encode(#{<<"chat">> => Text, <<"group">> => Group}),
            [send_msg(From, To, Msg) || To <- Members -- [From]],
            ok;
        false ->
            {error, not_member}
    end.

send_msg(From, To, Msg) ->
    case aeplugin_scm_registry:locate_endpoint(From) of
        undefined ->
            lager:debug("cannot find endpoint", []),
            {error, unknown_origin};
        Pid ->
            lager:debug("endpoint = ~p", [Pid]),
            {account, ToPub} = aeser_id:specialize(To),
            gen_server:call(Pid, {send_msg, ToPub, Msg})
    end.

send_payment(From, To, Amount, Msg) when is_integer(Amount)
                                         , Amount >= 0
                                         , is_binary(Msg) ->
    FromId = ensure_id(From),
    ToId = ensure_id(To),
    case {aeplugin_scm_registry:locate_endpoint(FromId),
          aeplugin_scm_registry:locate_endpoint(ToId)} of
        {undefined, _} ->
            {error, unknown_origin};
        {_, undefined} ->
            {error, unknown_recipient};
        {Pid, _} ->
            gen_server:call(Pid, {pay, ToId, Amount, Msg})
    end.

get_balances(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, get_balances).

%% not exported
forward_msg(From, To, Msg) ->
    case aeplugin_scm_registry:locate_market_side(To) of
        undefined ->
            {error, unknown_recipient};
        Pid ->
            {account, FromPub} = aeser_id:specialize(From),
            {account, ToPub} = aeser_id:specialize(To),
            gen_server:cast(Pid, {forward_msg, FromPub, ToPub, Msg})
    end.

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

handle_call({pay, To, Amount, Msg}, _From, St) ->
    HashLock = request_hashlock(pubkey(To), Amount, Msg, St),
    _AbsTimeout = new_send(To, Amount, HashLock, St),
    Res = await_contract_call(<<"collect">>, St),
    {reply, Res, St};
handle_call(get_balances, _From, #{fsm := Fsm} = St) ->
    IPub = initiator_pub(St),
    RPub = responder_pub(St),
    {ok, [{IPub,Bi},{RPub,Br}]} = aesc_fsm:get_balances(Fsm, [IPub, RPub]),
    {reply, {Bi, Br}, St};
handle_call(close_mutual, From, #{fsm := Fsm} = St) ->
    aesc_fsm:shutdown(Fsm, #{}),
    await_tx_to_sign(shutdown, St, ?TIMEOUT),
    gen_server:reply(From, ok),
    receive_on_chain_tx(channel_close_mutual_tx, close_mutual, St),
    {stop, normal, St};
handle_call({send_msg, To, Msg}, _From, St) ->
    do_send_msg(To, Msg, St),
    {reply, ok, St};
handle_call(_Rec, _From, St) ->
    {reply, {error, unknown_request}, St}.

handle_cast({forward_msg, From, To, Msg}, #{fsm := Fsm} = St) ->
    case my_role(St) of
        responder ->
            %% Don't touch the message
            ae_scm:info("~s: FWD MSG: ~s -> ~s - ~s", [my_nick(St),
                                                       abbrev_enc(From),
                                                       abbrev_enc(To),
                                                       truncate(Msg)]),
            aesc_fsm:inband_msg(Fsm, From, To, Msg),
            {noreply, St};
        initiator ->
            %% TODO: hand off to callback module or send report?
            {noreply, St}
    end;
handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({fwd_call, F, Args, Deposit}, St) ->
    contract_call(F, Args, Deposit, St),
    {noreply, St};
handle_info({aesc_fsm, Fsm, #{ tag  := message
                             , type := report
                             , notice := Notice } = Msg}, #{fsm := Fsm} = St) ->
    St1 = case {Notice, my_role(St)} of
              {please_forward, responder} ->
                  #{info := #{ from := FromPub
                             , to := ToPub
                             , info := Info}} = Msg,
                  ToId = aeser_id:create(account, ToPub),
                  case aeplugin_scm_registry:locate_endpoint(ToId) of
                      Pid when Pid =/= self() ->
                          ae_scm:debug("~s: FWD MSG ~s -> ~s - ~s",
                                       [my_nick(St), abbrev_enc(FromPub),
                                        abbrev_enc(ToPub),
                                        truncate(Info)]),
                          forward_msg(aeser_id:create(account, FromPub),
                                      aeser_id:create(account, ToPub), Info),
                          St;
                      _ ->
                          St
                  end;
              _ ->
                  #{info := #{ from := FromPub
                             , to := ToPub
                             , info := Info}} = Msg,
                  {ok, Dec} = decrypt_msg(Info, St),
                  ae_scm:debug("~s: MSG ~s -> ~s: ~s", [my_nick(St),
                                                        abbrev_enc(FromPub),
                                                        abbrev_enc(ToPub), Dec]),
                  handle_inband_msg(Dec, FromPub, ToPub, St)
          end,
    {noreply, St1};
handle_info({aesc_fsm, Fsm, #{ type := report
                             , tag  := info
                             , info := update } = Msg},
            #{ fsm := Fsm} = St) ->
    ae_scm:debug("UPDATE: ~p", [Msg]),
    {noreply, St};
handle_info({aesc_fsm, Fsm, #{ type := sign
                             , tag  := update_ack = Tag
                             , info := #{ signed_tx := SignedTx
                                        , updates := [Update]}}},
            #{fsm := Fsm} = St) ->
    ae_scm:debug("~s SIGN update_ack", [my_nick(St)]),
    %% TODO: aesc_offchain_update lacks an API to simply get the update type
    case {element(1, Update), my_role(St)} of
        {create_contract, initiator} ->
            sign_and_respond(Tag, SignedTx, St),
            %% We should perhaps extract the current round from the
            %% received offchain tx...
            {ok, Round} = aesc_fsm:get_round(Fsm),
            #{opts := #{responder := Owner}} = St,
            ContractPubkey = aect_contracts:compute_contract_pubkey(
                               Owner, Round),
            ae_scm:debug("~s: Contract loaded (~s). Msgs: ~p",
                         [my_nick(St), abbrev_enc(ContractPubkey),
                          msgs()]),
            #{ meta := ContractMeta
             , abi_version := AbiVsn } =
                aeplugin_scm_contract:contract_meta(),
            put(contract_meta, ContractMeta),  %% annoying to keep in state
            {noreply, St#{ contract_id => ContractPubkey
                         , contract_abi_version => AbiVsn }};
        {call_contract, responder} ->
            {Fun, Args} = extract_and_decode_call_data(Update),
            NewSignedTx = aeplugin_scm_signing:sign_tx(SignedTx, St),
            aesc_fsm:signing_response(Fsm, Tag, NewSignedTx),
            %% TODO: we really need to also inspect the call results
            forward_contract_call(Fun, Args, St),
            {noreply, St};
        {call_contract, initiator} ->
            ae_scm:debug("~s CONTRACT_CALL to initiator", [my_nick(St)]),
            case extract_and_decode_call_data(Update) of
                {<<"new_receive">>, [Amount, {address, FromPub},
                                     _AbsTimeout, {bytes, HashLock}]} ->
                    ae_scm:debug("~s NEW_RCV (~p, ~s, ...)", [my_nick(St),
                                                              Amount,
                                                              abbrev_enc(FromPub)]),
                    case St of
                        #{requests := #{HashLock := #{ from := FromPub
                                                     , amount := Amount
                                                     , secret := Secret }} = Reqs} ->
                            ae_scm:debug("~s FOUND transaction", [my_nick(St)]),
                            sign_and_respond(Tag, SignedTx, St),
                            {ok, _} =
                                contract_call(
                                  <<"receive">>,
                                  [FromPub, Amount, HashLock, Secret],
                                  0, St),
                            lager:info("~s GOT ~p from ~s",
                                       [my_nick(St),
                                        Amount,
                                        abbrev_enc(FromPub)]),
                            {noreply, St#{requests => maps:remove(HashLock, Reqs)}};
                        _ ->
                            aesc_fsm:signing_response(Fsm, Tag, {error, ?ERR_NOT_FOUND}),
                            {noreply, St}
                    end;
                _ ->
                    aesc_fsm:signing_response(Fsm, Tag, {error, ?ERR_UNKNOWN}),
                    {noreply, St}
            end;
        _ ->
            aesc_fsm:signing_response(Fsm, Tag, {error, ?ERR_FORBIDDEN}),
            {noreply, St}
    end;
handle_info({aesc_fsm, Fsm, #{ type := report
                             , tag  := shutdown }}, #{fsm := Fsm} = St) ->
    %% TODO: Check shutdown message for funds distribution
    await_tx_to_sign(shutdown_ack, St, ?TIMEOUT),
    receive_on_chain_tx(channel_close_mutual_tx, close_mutual, St),
    {stop, normal, St};
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
            %% TODO: The signing module is prepared to handle a signing
            %% fun as input, but for now, we rely on the privkey being
            %% provided for message encryption. This should be done in a
            %% more dynamic way.
            Sign = aeplugin_scm_signing:verify_signing_config(Sign0),
            #{privkey := PrivKey} = Sign0,
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
                                             , privkey => PrivKey
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
    receive_info_report(#{info => channel_accept}, St, timeout(accept, St)),
    SignedTx = await_tx_to_sign(create_tx, St, timeout(funding_create, St)),
    receive_info_report(#{info => funding_signed}, St,
                        timeout(funding_sign, St)),
    #{channel_id := ChId} =
        receive_on_chain_tx(channel_create_tx, funding_signed, St),
    finalize_channel_setup(St#{channel_id => ChId, create_tx => aetx_sign:innermost_tx(SignedTx)}).

init_channel_responder(St) ->
    receive_info_report(#{info => channel_open}, St, timeout(accept, St)),
    receive_info_report(#{info => funding_created}, St,
                        timeout(funding_create, St)),
    await_tx_to_sign(funding_created, St, timeout(funding_create, St)),
    #{channel_id := ChId} =
        receive_on_chain_tx(channel_create_tx, funding_created, St),
    finalize_channel_setup(St#{channel_id => ChId}).

%% Common for both initiator and responder
%%
finalize_channel_setup(St0) ->
    #{info := #{tx := SignedTx}} =
        receive_on_chain_tx(channel_create_tx, channel_changed, St0),
    St = get_pubkeys_from_signed_create_tx(SignedTx, St0),
    receive_info_report(#{info => own_funding_locked}, St, ?LONG_TIMEOUT),
    lager:info("Awaiting min_depth confirmation (~p blocks)", [get_opt(minimum_depth, St)]),
    receive_info_report(#{info => funding_locked}, St),
    receive_info_report(#{info => open}, St),
    receive_report(update, St),
    aeplugin_scm_registry:register_session(St),
    St1 = maybe_load_contract(St),
    ae_scm:debug("~p: channel open (~s)", [my_role(St1), my_nick(St1)]),
    ae_scm:debug("~p: msgQ: ~p", [my_role(St1), msgs()]),
    St1.

get_opt(Key, #{opts := Opts}) ->
    maps:get(Key, Opts, undefined).

sign_and_respond(Tag, SignedTx, #{fsm := Fsm} = St) ->
    NewSignedTx = aeplugin_scm_signing:sign_tx(SignedTx, St),
    aesc_fsm:signing_response(Fsm, Tag, NewSignedTx),
    _Info = receive_report(update, St),
    ok.

receive_info_report(Info, St) ->
    receive_info_report(Info, St, ?TIMEOUT).

receive_info_report(Info, St, Timeout) ->
    receive_report(info, Info, St, Timeout).

receive_report(Tag, St) ->
    receive_report(Tag, #{}, St).

receive_report(Tag, Pat, St) ->
    receive_report(Tag, Pat, St, ?TIMEOUT).

receive_report(Tag, Pat, #{fsm := Fsm} = St, Timeout) when is_map(Pat) ->
    receive
        {aesc_fsm, Fsm, #{ type := report
                         , tag  := Tag } = Msg} ->
            case match_info_msg(Pat, Msg) of
                ok ->
                    Msg
            end
    after Timeout ->
            lager:debug("~p TIMEOUT - Msgs: ~p", [my_role(St), msgs()]),
            error({timeout, {info_report, Pat}})
    end.

match_info_msg(Pat, Msg) ->
    maps:fold(fun(K, V, Acc) ->
                      case maps:find(K, Msg) of
                          {ok, V1} when is_map(V), is_map(V1) ->
                              match_info_msg(V, V1);
                          {ok, V} ->
                              Acc;
                          {ok, Other} ->
                              error({info_mismatch, {K, {V, Other}}});
                          error ->
                              error({no_such_key, K})
                      end
              end, ok, Pat).

receive_on_chain_tx(TxType, Info, #{fsm := Fsm} = St) ->
    ae_scm:debug("~p: awaiting on_chain_tx ~p (~p)", [my_role(St), TxType, Info]),
    receive
        {aesc_fsm, Fsm, #{ type := report
                         , tag  := on_chain_tx
                         , info := #{ info := Info
                                    , type := TxType
                                    , tx   := _SignedTx } } = Msg} ->
            Msg
    after ?LONG_TIMEOUT ->
            lager:debug("~p TIMEOUT - Msgs: ~p", [my_role(St), msgs()]),
            error({timeout, {on_chain_tx, TxType, Info}})
    end.

await_tx_to_sign(Tag, #{fsm := Fsm} = St, Timeout) ->
    ae_scm:debug("~p: awaiting signing req (~p)", [my_role(St), Tag]),
    receive
        {aesc_fsm, Fsm, #{ type := sign
                         , tag  := Tag
                         , info := #{signed_tx := SignedTx} }} ->
            NewSignedTx = aeplugin_scm_signing:sign_tx(SignedTx, St),
            aesc_fsm:signing_response(Fsm, Tag, NewSignedTx),
            NewSignedTx
    after Timeout ->
            lager:debug("~p TIMEOUT - Msgs: ~p", [my_role(St), msgs()]),
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

my_role(#{opts := #{role := Role}}) ->
    Role.

my_nick(St) ->
    abbrev_enc(my_pub(St)).

my_pub(#{opts := #{role := Role} = Opts}) ->
    maps:get(Role, Opts).

initiator_pub(#{ids := #{initiator := IId}}) ->
    {account, Pub} = aeser_id:specialize(IId),
    Pub.

responder_pub(#{opts := #{responder := R}}) ->
    R.

abbrev_enc(K) ->
    Enc = aeser_api_encoder:encode(account_pubkey, K),
    aeplugin_scm_util:abbrev_key(Enc).

truncate(Str) ->
    case byte_size(Str) > 19 of
        true ->
            << (binary:part(Str,0,15))/binary, " ..." >>;
        false ->
            Str
    end.

msgs() ->
    {_, Msgs} = process_info(self(), messages),
    Msgs.

encrypt_msg(Msg, TheirPub0) ->
    TheirPub = pubkey(TheirPub0),
    EncPub = enacl:crypto_sign_ed25519_public_to_curve25519(TheirPub),
    enacl:box_seal(Msg, EncPub).

decrypt_msg(Msg, St) ->
    MyPub = my_pub(St),
    MyPriv = my_priv(St),
    EncPub = enacl:crypto_sign_ed25519_public_to_curve25519(MyPub),
    DecPriv = enacl:crypto_sign_ed25519_secret_to_curve25519(MyPriv),
    enacl:box_seal_open(Msg, EncPub, DecPriv).

maybe_load_contract(St) ->
    case my_role(St) of
        responder ->
            #{ids := #{initiator := IId}, fsm := Fsm} = St,
            {ok, Round0} = aesc_fsm:get_round(Fsm),
            {account, IPub} = aeser_id:specialize(IId),
            Enc = aeser_api_encoder:encode(account_pubkey, IPub),
            #{ contract_meta := ContractMeta
             , create_args   := CreateArgs} =
                aeplugin_scm_contract:create_args(Enc),
            #{abi_version := AbiVersion} = CreateArgs,
            put(contract_meta, ContractMeta),  %% annoying to keep in state
            aesc_fsm:upd_create_contract(Fsm, CreateArgs),
            await_tx_to_sign(update, St, ?TIMEOUT),
            receive_report(update, St),
            #{opts := #{responder := Owner}} = St,
            ContractPubkey = aect_contracts:compute_contract_pubkey(
                               Owner, Round0 + 1),
            ae_scm:debug("~s: Contract loaded (~s)",
                         [my_nick(St), abbrev_enc(ContractPubkey)]),
            St#{ contract_id => ContractPubkey
               , contract_abi_version => AbiVersion };
        _ ->
            St
    end.

pubkey(<<"ak_", _/binary>> = Enc) ->
    {ok, PK} = aeser_api_encoder:safe_decode(account_pubkey, Enc),
    PK;
pubkey(Pub) when is_binary(Pub) ->
    Pub;
pubkey(Id) ->
    case aeser_id:is_id(Id) of
        true ->
            {account, Pub} = aeser_id:specialize(Id),
            Pub;
        false ->
            error(invalid_pubkey)
    end.

ensure_id(X) ->
    case aeser_id:is_id(X) of
        true ->
            X;
        false ->
            aeser_id:create(account, pubkey(X))
    end.

my_priv(#{privkey := Priv}) ->
    Priv.

do_send_msg(To, Msg, #{fsm := Fsm} = St) ->
    ae_scm:info("SND MSG: ~s -> ~s - ~s", [my_nick(St), abbrev_enc(To),
                                           truncate(Msg)]),
    Encrypted = encrypt_msg(Msg, To),
    aesc_fsm:inband_msg(Fsm, To, Encrypted).

request_hashlock(To, Amount, Msg, St) ->
    Seq = erlang:unique_integer([positive, monotonic]),
    Req = #{ <<"req">> => <<"SEND">>
           , <<"id">>  => Seq
           , <<"amount">> => Amount
           , <<"msg">> => Msg },
    do_send_msg(To, jsx:encode(Req), St),
    #{ <<"reply">> := <<"OK">>
     , <<"id">> := Seq
     , <<"hash_lock">> := HashLockEnc } =
        try_jsx_decode(receive_and_decrypt_msg(To, St)),
    {ok, HashLock} = aeser_api_encoder:safe_decode(bytearray, HashLockEnc),
    HashLock.

send_hashlock_reply(Id, To, St) ->
    Secret = crypto:strong_rand_bytes(32),
    HashLock = crypto:hash(sha256, Secret),
    Msg = #{ <<"reply">> => <<"OK">>
           , <<"id">>    => Id
           , <<"hash_lock">> => aeser_api_encoder:encode(
                                  bytearray, HashLock) },
    do_send_msg(To, jsx:encode(Msg), St),
    #{ hash_lock => HashLock, secret => Secret }.

receive_and_decrypt_msg(From, #{fsm := Fsm} = St) ->
    receive
        {aesc_fsm, Fsm, #{ tag := message
                         , type := report
                         , info := #{ from := From
                                    , info := Msg }}} ->
            {ok, Decrypted} = decrypt_msg(Msg, St),
            Decrypted
    after ?LONG_TIMEOUT ->
            {error, {timeout, receive_inband_msg}}
    end.

try_jsx_decode(Msg) ->
    try jsx:decode(Msg, [return_maps])
    catch
        error:_ ->
            Msg
    end.

handle_inband_msg(Msg, From, To, St) ->
    try jsx:decode(Msg, [return_maps]) of
        #{ <<"req">> := _
         , <<"id">>  := _ } = Request ->
            handle_request(Request, From, St);
        #{ <<"chat">> := Text } = Chat ->
            ToAbbr = abbrev_enc(To),
            Target = maps:get(<<"group">>, Chat, ToAbbr),
            Hdr = [abbrev_enc(From), " -> ", Target],
            ae_scm_chat:info("~s~n~s", [Hdr, Text]),
            St;
        _ ->
            %% TODO: pass on to user
            St
    catch
        error:_ ->
            %% TODO: pass on to user
            St
    end.

handle_request(#{ <<"req">> := <<"SEND">>
                , <<"id">> := Id
                , <<"amount">> := Amount
                , <<"msg">> := Msg }, From, St) ->
    #{hash_lock := HashLock} = Params = send_hashlock_reply(Id, From, St),
    Reqs0 = maps:get(requests, St, #{}),
    Reqs = Reqs0#{ HashLock => Params#{ from => From
                                      , amount => Amount
                                      , msg => Msg } },
    St#{requests => Reqs}.

new_send(To, Amount, HashLock, St) ->
    Fee = aeplugin_scm_contract:fee(),
    Timeout = aeplugin_scm_contract:timeout(),
    {ok, {integer, AbsTimeout}} =
        contract_call(
          <<"new_send">>, [Amount, To, Fee, Timeout, HashLock],
          Amount + Fee, St),
    AbsTimeout.

contract_call(F, Args, Amount, St) ->
    #{ fsm := Fsm
     , contract_id := ContractId
     , contract_abi_version := AbiVsn } = St,
    Meta = get(contract_meta),
    {ok, CallData} = aefa_fate_code:encode_calldata(
                       maps:get(fate_code, Meta), F, Args),
    CallArgs = #{ contract      => ContractId
                , abi_version   => AbiVsn
                , amount        => Amount
                , call_data     => CallData
                , return_result => true },
    {ok, CallRes} = aesc_fsm:upd_call_contract(Fsm, CallArgs),
    await_tx_to_sign(update, St, ?TIMEOUT),
    receive_report(update, St),
    decode_callres(CallRes, F, Meta).

await_contract_call(F, #{fsm := Fsm} = St) ->
    receive
        {aesc_fsm, Fsm, #{ type := sign
                         , tag  := update_ack = Tag
                         , info := #{ signed_tx := SignedTx
                                    , updates := [Update]}}}
          when element(1, Update) =:= call_contract ->
            CallData = aesc_offchain_update:extract_call_data(Update),
            {F, _Args} = fate_code_decode_call_data(CallData),
            NewSignedTx = aeplugin_scm_signing:sign_tx(SignedTx, St),
            aesc_fsm:signing_response(Fsm, Tag, NewSignedTx),
            _ = receive_report(update, St),
            ok
    after ?TIMEOUT ->
            error({timeout, {await_contract_call, F}})
    end.

forward_contract_call(F, Args, St) ->
    forward_contract_call(F, Args, my_role(St), St).

forward_contract_call(<<"new_send">>, Args, responder, St) ->
    [Amount, {address, ToPub}, _Fee, Timeout, {bytes, HashLock}] = Args,
    ToId = aeser_id:create(account, ToPub),
    %% TODO: AbsTimeout is supposed to be taken from the return value of new_send()
    AbsTimeout = aec_chain:top_height() + Timeout,
    IPub = initiator_pub(St),
    try_forward_call(ToId, <<"new_receive">>,
                     [Amount, IPub, AbsTimeout, HashLock], Amount);
forward_contract_call(<<"receive">>, Args, responder, St) ->
    [{address, FromPub}, Amount, {bytes, HashLock}, {bytes, Secret}] = Args,
    FromId = aeser_id:create(account, FromPub),
    IPub = initiator_pub(St),
    try_forward_call(FromId, <<"collect">>,
                     [IPub, Amount, HashLock, Secret], 0).

try_forward_call(ToId, F, Args, Deposit) ->
    case aeplugin_scm_registry:locate_market_side(ToId) of
        undefined ->
            ae_scm:debug("Cannot forward ~s call to ~s",
                         [F, abbrev_enc(pubkey(ToId))]);
        Pid ->
            Pid ! {fwd_call, F, Args, Deposit}
    end.

%% caller_and_validator(#{ opts := #{role := Role}
%%                       , ids := #{ initiator := I
%%                                 , responder := R}}) ->
%%     Ki = pubkey(I), Kr = pubkey(R),
%%     case Role of
%%         initiator -> {Ki, Kr};
%%         responder -> {Kr, Ki}
%%     end.

%% NOTE: the `unit' data type comes back as `{{tuple, []}, {tuple, {}}}'
%%
decode_callres({ok, Value}, F, Meta) ->
    {ok, aefa_fate_code:decode_result(maps:get(fate_code, Meta), F, Value)};
decode_callres({error, Reason}, _, _) ->
    {error, Reason};
decode_callres({revert, Reason}, _F, _Meta) ->
    {error, aeb_fate_encoding:deserialize(Reason)}.

extract_and_decode_call_data(Update) ->
    CallData = aesc_offchain_update:extract_call_data(Update),
    {_Fun, _Args} = fate_code_decode_call_data(CallData).

%% TODO: Provide a generalized version in aefa_fate_code.erl
%%
fate_code_decode_call_data(CallData) ->
    {tuple, {Hash, {tuple, Args}}} =
        aeb_fate_encoding:deserialize(CallData),
    ContractMeta = get(contract_meta),
    FateCode = maps:get(fate_code, ContractMeta),
    Fname = maps:get(Hash, aeb_fate_code:symbols(FateCode)),
    Decoded = {Fname, tuple_to_list(Args)},
    ae_scm:debug("Decoded = ~p", [Decoded]),
    Decoded.
