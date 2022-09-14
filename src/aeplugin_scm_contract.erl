-module(aeplugin_scm_contract).
-behavior(gen_server).

-export([ contract_meta/0
        , create_args/1
        , create_args/4 ]).

-export([ fee/0
        , timeout/0 ]).

-export([ start_link/0
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).

-define(MINIMUM_FEE, 100).
-define(DEFAULT_TIMEOUT, 3).
-define(DEPOSIT, 10).

%% TODO: Fetch these values in a robust way
%% (They reside in aecontract/include/aecontract.hrl, but are protocol-dependent)
-define(ABI_FATE_SOPHIA_1, 16#03).
-define(VM_FATE_SOPHIA_3, 16#08).

contract_meta() ->
    #{ contract_meta := ContractMeta
     , create_args := #{abi_version := AbiVersion} } =
        gen_server:call(?MODULE, get_contract),
    #{ meta => ContractMeta
     , abi_version => AbiVersion }.

create_args(EncodedPub) ->
    create_args(EncodedPub, ?MINIMUM_FEE, ?DEFAULT_TIMEOUT, ?DEPOSIT).

create_args(EncodedPub, Fee, Timeout, Deposit) ->
    #{ contract_meta := ContractMeta
     , create_args   := CreateArgs0 } = Meta
        = gen_server:call(?MODULE, get_contract),
    {ok, CallData} = aefa_fate_code:encode_calldata(
                       maps:get(fate_code, ContractMeta),
                       <<"init">>, [EncodedPub, Fee, Timeout]),
    Meta#{ create_args := CreateArgs0#{ deposit => Deposit
                                      , call_data => CallData } }.

fee() ->
    ?MINIMUM_FEE.

timeout() ->
    ?DEFAULT_TIMEOUT.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    CompileRes = compile_contract(),
    CreateArgs = contract_create_args(CompileRes),
    {ok, #{ contract_meta => CompileRes
          , create_args   => CreateArgs }}.

handle_call(get_contract, _From, St) ->
    {reply, St, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

compile_contract() ->
    Fname = filename:join(code:priv_dir(aeplugin_scm), "channel_htlc.aes"),
    {ok, Res} = aeso_compiler:file(Fname, [{backend, fate}]),
    Res.

contract_create_args(CompileRes) ->
    Code = aect_sophia:serialize(CompileRes, _SophiaVsn = 3),
    %% Need to specialize by adding deposit and call_data
    #{ vm_version  => ?VM_FATE_SOPHIA_3
     , abi_version => ?ABI_FATE_SOPHIA_1
     , code        => Code }.

contract_call(ContractId, F, Args, Amount, A, B, Meta, Cfg) ->
    {ok, CallData} = aefa_fate_code:encode_calldata(
                       maps:get(fate_code, Meta), F, Args),
    CallArgs = #{ contract      => ContractId
                , abi_version   => aect_test_utils:abi_version()
                , amount        => Amount
                , call_data     => CallData
                , return_result => true },
    {A1, B1, CallRes} =
        aesc_fsm_SUITE:upd_call_contract(A, B, CallArgs, Cfg),
    {A1, B1, decode_callres(CallRes, F, Meta)}.

%% NOTE: the `unit' data type comes back as `{{tuple, []}, {tuple, {}}}'
%%
decode_callres({ok, Value}, F, Meta) ->
    {ok, aefa_fate_code:decode_result(maps:get(fate_code, Meta), F, Value)};
decode_callres({error, Reason}, _, _) ->
    {error, Reason};
decode_callres({revert, Reason}, _F, _Meta) ->
    {error, aeb_fate_encoding:deserialize(Reason)}.
