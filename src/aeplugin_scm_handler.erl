%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
-module(aeplugin_scm_handler).

-export([routes/0]).

-export([ init/2
        , content_types_provided/2
        , to_html/2
        , json_api/2
        ]).

-import(aeplugin_scm_html, [html/1, meta/0]).

routes() ->
    [
     {'_', [ {"/", ?MODULE, []}
           , {"/connect_customer/", ?MODULE, []}
           , {"/connect_merchant/", ?MODULE, []}
           , {"/customer/", ?MODULE, []}
           , {"/send_chat_msg/", ?MODULE, []}
           , {"/send_payment/", ?MODULE, []}
           , {"/seed_accounts/", ?MODULE, []}
           , {"/status", ?MODULE, []}
           ]}
    ].

init(Req, Opts) ->
    lager:debug("Req = ~p, Opts = ~p", [Req, Opts]),
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {Result, Req1} = case serve_request(Req) of
                         ok -> {ok, Req};
                         {ok, _} = Ok -> Ok
                     end,
    {[
       {<<"text/html">>, to_html},
       {<<"application/json">>, json_api}
     ], Req1#{'$result' => Result}, State}.

json_api(#{'$result' := Result, qs := Qs} = Req, State) ->
    Response0 = case Result of
                    ok ->
                        #{ <<"result">> => <<"ok">> };
                    {error, Reason} ->
                        #{ <<"error">> => to_bin(Reason)};
                    Map when is_map(Map) ->
                        Map
                end,
    Response = chain_status(Response0),
    JSON = parse_qs(Qs, [{<<"pp_json">>, boolean, false}],
                    fun(false) ->
                            jsx:encode(Response);
                       (true) ->
                            jsx:encode(Response, [{indent, 2}])
                    end),
    {JSON, Req, State}.

to_html(Req, State) ->
    lager:debug("to_html called", []),
    Merchants = [{M,
                  aeplugin_scm_registry:names_by_id(M),
                  aeplugin_scm_registry:tags_by_id(M)}
                 || M <- aeplugin_scm_registry:list_merchants()],
    Customers = aeplugin_scm_registry:list_customers(),
    Balances = account_balances(),
    HTML = html(
             {html,
              [{head, [meta(),
                       {title, <<"AE SC Market Demo">>},
                       {style, <<("table, th, td {"
                                  "border: 1px solid black;"
                                  "border-collapse: collapse;")>>}
                      ]},
               {body,
                [{a, #{href => <<"/">>}, <<"home">>},
                 {h2, <<"Merchants">>},
                 merchant_table(Merchants, Balances),
                 {h2, <<"Customers">>},
                 customer_table(Customers, Balances),
                 {p, []},
                 {p, [], <<"Total Market Balance: ", (total_market_balance())/binary>>},
                 {h2, <<"Actions">>},
                 {a, #{href => <<"/seed_accounts">>}, <<"Seed Accounts">>},
                 {p, []},
                 chat_form(Customers, Merchants),
                 payment_form(Customers, Merchants),
                 connect_customer_form(),
                 connect_merchant_form()
                ]}
              ]}),
    {HTML, Req, State}.

total_market_balance() ->
    Sum = lists:foldl(
            fun(Pid, Acc) ->
                    {_, Bal} = aeplugin_scm_sc:get_balances(Pid),
                    Bal + Acc
            end, 0, aeplugin_scm_registry:market_responder_pids()),
    integer_to_binary(Sum).

merchant_table(Merchants, Balances) ->
    {table,
     [{tr, [{th, <<"Id">>},
            {th, <<"Name(s)">>},
            {th, <<"Tags">>},
            {th, <<"On-Chain Bal">>},
            {th, <<"Channel Bal">>},
            {th, <<"Market Bal">>}]}
      | lists:map(
          fun({M, Ns, Ts}) ->
                  case aeplugin_scm_registry:locate_endpoint(M) of
                      undefined -> [];
                      Pid ->
                          {account, PK} = aeser_id:specialize(M),
                          EncPK = aeser_api_encoder:encode(account_pubkey, PK),
                          {Bi, Br} = aeplugin_scm_sc:get_balances(Pid),
                          {tr, [{td, aeplugin_scm_util:abbrev_key(EncPK)},
                                {td, intersperse(Ns)},
                                {td, intersperse(Ts)},
                                {td, account_balance(PK, Balances)},
                                {td, integer_to_binary(Bi)},
                                {td, integer_to_binary(Br)}]}
                  end
          end, Merchants)]}.

customer_table(Customers, Balances) ->
    {table,
     [{tr, [{th, <<"Id">>},
            {th, <<"On-Chain Bal">>},
            {th, <<"Channel Bal">>},
            {th, <<"Market, Bal">>}]}
      | lists:map(
          fun(C) ->
                  case aeplugin_scm_registry:locate_endpoint(C) of
                      undefined -> [];
                      Pid ->
                          {account, PK} = aeser_id:specialize(C),
                          EncPK = aeser_api_encoder:encode(account_pubkey, PK),
                          {Bi, Br} = aeplugin_scm_sc:get_balances(Pid),
                          {tr, [{td, aeplugin_scm_util:abbrev_key(EncPK)},
                                {td, account_balance(PK, Balances)},
                                {td, integer_to_binary(Bi)},
                                {td, integer_to_binary(Br)}]}
                  end
          end, Customers)]}.

full_options_list(Customers, Merchants) ->
    COptions = [customer_option(C) || C <- Customers],
    MOptions = lists:flatmap(fun merchant_options/1, Merchants),
    COptions ++ [opt_divider(<<"Merchants">>) | MOptions].

opt_divider(Txt) ->
    {option, #{disabled => true}, <<"-- ", Txt/binary, " --">>}.

customer_option(C) ->
    Enc = encode_id(C),
    {option, #{value => Enc}, abbrev(Enc)}.

merchant_options({M, Names, _Tags}) ->
    Enc = encode_id(M),
    [{option, #{value => V}, N} || {V, N} <- [{Enc, abbrev(Enc)}
                                             | [{Enc, N} || N <- Names]]].

chat_form([], []) -> [];
chat_form(Customers, Merchants) ->
    Groups = aeplugin_scm_registry:list_chat_groups(),
    FullOptions = full_options_list(Customers, Merchants),
    GroupOptions = [{option, #{value => <<"grp:", G/binary>>}, G}
                    || G <- Groups],
    {form, #{action => <<"/send_chat_msg">>, method => get},
     [{label, #{for => from}, <<"From: ">>},
      {select, #{name => from, id => from}, FullOptions},
      {label, #{for => to}, <<"To: ">>},
      {select, #{name => to, id => to}, FullOptions ++ [opt_divider(<<"Groups">>)
                                                        | GroupOptions]},
      {label, #{for => msg}, <<"Msg: ">>},
      {input, #{type => text, id => msg, name => msg}, []},
      {input, #{type => submit, value => <<"Send">>}, []}
     ]}.

payment_form([], _) -> [];
payment_form(_, []) -> [];
payment_form(Customers, Merchants) ->
    COptions = [customer_option(C) || C <- Customers],
    MOptions = lists:flatmap(fun merchant_options/1, Merchants),
    {form, #{action => <<"/send_payment">>, method => get},
     [{label, #{for => from}, <<"From: ">>},
      {select, #{name => from, id => from}, COptions},
      {label, #{for => to}, <<"To: ">>},
      {select, #{name => to, id => to}, MOptions},
      {label, #{for => amount}, <<"Amount: ">>},
      {input, #{type => text, id => amount, name => amount}, []},
      {label, #{for => msg}, <<"Msg: ">>},
      {input, #{type => text, id => msg, name => msg}, []},
      {input, #{type => submit, value => <<"Send Payment">>}, []}
     ]}.

connect_customer_form() ->
    EncIds = lists:map(fun encode_id/1, available_ids()),
    Options = [{option, #{value => Enc}, Enc} || Enc <- EncIds],
    {form, #{action => <<"/connect_customer">>, method => get},
     [{label, #{for => cid}, <<"Id: ">>},
      {select, #{name => cid, id => cid}, Options},
      {input, #{type => submit, value => <<"Connect Customer">>}, []}
     ]}.

connect_merchant_form() ->
    EncIds = lists:map(fun encode_id/1, available_ids()),
    Options = [{option, #{value => Enc}, Enc} || Enc <- EncIds],
    {form, #{action => <<"/connect_merchant">>, method => get},
     [{label, #{for => mid}, <<"Id: ">>},
      {select, #{name => mid, id => mid}, Options},
      {label, #{for => names}, <<"Names: ">>},
      {input, #{type => text, id => names, name => names}, []},
      {label, #{for => tags}, <<"Tags: ">>},
      {input, #{type => text, id => tags, name => tags}, []},
      {input, #{type => submit, value => <<"Connect Merchant">>}, []}
     ]}.

encode_id(Id) ->
    {account, Pub} = aeser_id:specialize(Id),
    aeser_api_encoder:encode(account_pubkey, Pub).

abbrev(Str) ->
    aeplugin_scm_util:abbrev_key(Str).

available_ids() ->
    Viable = viable_ids(),
    [Id || Id <- Viable,
           not customer_or_merchant(Id)].

customer_or_merchant(Id) ->
    (aeplugin_scm_registry:locate_customer(Id) =/= undefined)
        orelse
        (aeplugin_scm_registry:locate_merchant(Id) =/= undefined).

viable_ids() ->
    DemoPairs0 = demo_keypairs(),
    DemoPairs = lists:keydelete(
                  aeplugin_scm_server:market_pubkey(), 1, DemoPairs0),
    Priv = fun(Pub) ->
                   proplists:get_value(Pub, DemoPairs, <<>>)
           end,
    Accts = [{Pub, Priv(Pub), Bal}
             || {Pub, Bal} <- account_balances(),
                Bal > 100000000000000],
    [aeser_id:create(account, Pu) || {Pu,Pr,_} <- Accts, Pr =/= <<>>].

serve_request(#{path := <<"/connect_customer">>, qs := Qs}) ->
    lager:debug("connect_customer", []),
    Params = parse_query(Qs),
    lager:debug("Params = ~p", [Params]),
    case proplists:get_value(<<"cid">>, Params, undefined) of
        undefined -> ok;
        Id ->
            {ok, CPub} = aeser_api_encoder:safe_decode(account_pubkey, Id),
            CId = aeser_id:create(account, CPub),
            lager:debug("CId = ~p", [CId]),
            case lists:member(CId, available_ids()) of
                true ->
                    lager:debug("CId is available", []),
                    KeyPairs = demo_keypairs(),
                    {PubK, PrivK} = lists:keyfind(CPub, 1, KeyPairs),
                    {Host, Port} = aeplugin_scm_server:market_endpoint(),
                    ChOpts = aeplugin_scm_util:channel_opts(
                               initiator, #{pubkey => PubK,
                                            privkey => PrivK},
                               aeplugin_scm_server:market_pubkey()),
                    aeplugin_scm_sc:initiate(Host, Port, ChOpts);
                false ->
                    lager:debug("CId (~p) NOT available", [CId]),
                    ok
            end
    end,
    ok;
serve_request(#{path := <<"/connect_merchant">>, qs := Qs}) ->
    lager:debug("connect_merchant", []),
    Params = parse_query(Qs),
    lager:debug("Params = ~p", [Params]),
    case [proplists:get_value(K, Params, Default)
          || {K, Default} <- [ {<<"mid">>, undefined}
                             , {<<"names">>, <<>>}
                             , {<<"tags">>, <<>>}]] of
        [undefined | _] ->
            lager:debug("No MId - ignoring", []),
            ok;
        [Id, NamesStr, TagsStr] ->
            {ok, MPub} = aeser_api_encoder:safe_decode(account_pubkey, Id),
            MId = aeser_id:create(account, MPub),
            lager:debug("MId = ~p", [MId]),
            case lists:member(MId, available_ids()) of
                true ->
                    lager:debug("MId available", []),
                    KeyPairs = demo_keypairs(),
                    {PubK, PrivK} = lists:keyfind(MPub, 1, KeyPairs),
                    {Host, Port} = aeplugin_scm_server:market_endpoint(),
                    ChOpts = aeplugin_scm_util:channel_opts(
                               initiator, #{pubkey => PubK,
                                            privkey => PrivK},
                               aeplugin_scm_server:market_pubkey()),
                    Names = aeplugin_scm_util:split_words(NamesStr),
                    Tags = aeplugin_scm_util:split_words(TagsStr),
                    aeplugin_scm_sc:initiate(
                      Host, Port,
                      ChOpts#{register_as => #{names => Names,
                                               tags => Tags}});
                false ->
                    lager:debug("MId NOT available (~p)", [MId]),
                    ok
            end
    end,
    ok;
serve_request(#{path := <<"/send_chat_msg">>} = Req) ->
    lager:debug("Send chat. Qs = ~p", [parse_query(maps:get(qs, Req))]),
    #{from := From, to := To, msg := Msg} = Matched =
        cowboy_req:match_qs([{P,nonempty} || P <- [from,to,msg]], Req),
    lager:debug("Matched = ~p", [Matched]),
    FromId = enc2id(From),
    case To of
        <<"grp:", G/binary>> ->
            lager:debug("Send to group chat (~p)", [G]),
            aeplugin_scm_sc:group_chat(FromId, G, Msg);
        _ ->
            lager:debug("Direct chat to ~s", [To]),
            ToId = enc2id(To),
            aeplugin_scm_sc:chat(FromId, ToId, Msg)
    end,
    ok;
serve_request(#{path := <<"/send_payment">>} = Req) ->
    lager:debug("Send payment. Qs ~p", [parse_query(maps:get(qs,Req))]),
    #{from := From, to := To, amount := Amount, msg := Msg} = Matched =
        cowboy_req:match_qs([{from, nonempty},
                             {to, nonempty},
                             {amount, int},
                             msg], Req),
    lager:debug("Matched = ~p", [Matched]),
    case Amount > 0 of
        true ->
            FromId = enc2id(From),
            ToId = enc2id(To),
            aeplugin_scm_sc:send_payment(FromId, ToId, Amount, Msg);
        false ->
            ignore
    end,
    ok;
serve_request(#{path := <<"/customer">>, qs := Qs}) ->
    Params = parse_query(Qs),
    case proplists:get_value(<<"cid">>, Params, undefined) of
        undefined ->
            ok;
        _CId ->
            ok
    end;
serve_request(#{path := <<"/seed_accounts">>} = Req) ->
    lager:debug("seed_accounts", []),
    Amount = 10000000000000000,
    DemoKeyPairs = demo_keypairs(),
    aedevmode_emitter:emit_keyblocks(5),
    Beneficiary = aec_headers:beneficiary(aec_chain:top_header()),
    lager:debug("Beneficiary: ~p", [Beneficiary]),
    {_, BPriv} = lists:keyfind(Beneficiary, 1, DemoKeyPairs),
    lager:debug("Found beneficiary privkey", []),
    Bals = account_balances(),
    lager:debug("Bals = ~p", [Bals]),
    Pubs = lists:foldr(
             fun({PK,_}, Acc) ->
                     case lists:keyfind(PK, 1, Bals) of
                         {_, Bal} when Bal > Amount ->
                             Acc;
                         _ ->
                             [PK | Acc]
                     end
             end, [], DemoKeyPairs),
    lager:debug("Pubs to spend to: ~p", [Pubs]),
    {ok, Nonce} = aec_next_nonce:pick_for_account(Beneficiary),
    lager:debug("next nonce: ~p", [Nonce]),
    From = acct(Beneficiary),
    {Hashes, _} =
        lists:mapfoldl(
          fun(To, N) ->
                  {ok, Tx} = aec_spend_tx:new(
                               #{sender_id => From,
                                 recipient_id => acct(To),
                                 amount => Amount,
                                 nonce => N,
                                 fee => 20000 * min_gas_price(),
                                 ttl => 0,
                                 payload => <<"scm_demo">>}),
                  lager:debug("Tx = ~p", [Tx]),
                  STx = sign_tx(Tx, BPriv),
                  lager:debug("Tx signed", []),
                  Res = aec_tx_pool:push(STx),
                  lager:info("Push Result = ~p", [Res]),
                  {aetx_sign:hash(STx), N+1}
          end, Nonce, Pubs),
    aedevmode_emitter:mine_until_txs_on_chain(Hashes, 10),
    {ok, Req#{path => <<"/">>}};
serve_request(_) ->
    ok.

enc2id(Enc) ->
    {ok, Pub} = aeser_api_encoder:safe_decode(account_pubkey, Enc),
    aeser_id:create(account, Pub).

parse_qs(Qs, Types, F) ->
    Params = parse_query(Qs),
    try lists:map(
          fun(PSpec) ->
                  case parse_param(PSpec, Params) of
                      {ok, Val} ->
                          Val;
                      Error ->
                          lager:info("Bad parameter: ~p -> ~p", [PSpec, Error]),
                          throw(parse_error)
                  end
          end, Types) of
        Vals ->
            apply(F, Vals)
    catch
        throw:parse_error ->
            ignore
    end.

parse_param({Name, Type, Default}, Params) ->
    case lists:keyfind(Name, 1, Params) of
        {_, Val0} ->
            check_type(Val0, Type, Name);
        false ->
            {ok, Default}
    end.

check_type(V, integer, Name) ->
    try {ok, binary_to_integer(V)}
    catch
        error:_ ->
            {error, {not_an_integer, V, Name}}
    end;
check_type(V, boolean, _Name) ->
    {ok, not lists:member(V, [<<"0">>, <<"false">>])}.

sign_tx(Tx, PrivKey) ->
    Bin = aetx:serialize_to_binary(Tx),
    BinForNetwork = aec_governance:add_network_id(Bin),
    Sigs = [ enacl:sign_detached(BinForNetwork, PrivKey) ],
    aetx_sign:new(Tx, Sigs).

acct(Key) ->
    aeser_id:create(account, Key).

min_gas_price() ->
    aec_tx_pool:minimum_miner_gas_price().

%% [{Pubkey, Privkey}]
demo_keypairs() ->
    [patron_keypair(),
     {<<34,211,105,168,28,63,144,218,27,148,69,230,108,203,60,
        118,189,48,67,20,68,151,186,192,77,185,248,60,73,145,
        254,193>>,
      <<251,183,173,174,69,15,7,18,184,88,101,70,33,94,137,156,
        241,33,30,29,169,80,68,174,19,172,112,177,60,30,238,
        119,34,211,105,168,28,63,144,218,27,148,69,230,108,203,
        60,118,189,48,67,20,68,151,186,192,77,185,248,60,73,
        145,254,193>>},
     {<<9,129,74,44,23,8,42,187,74,233,50,244,189,191,90,204,
        249,158,85,249,95,181,221,222,218,245,88,233,171,237,88,
        128>>,
      <<105,106,180,148,180,188,74,140,12,220,124,26,175,225,
        161,192,231,132,93,61,132,150,103,130,124,108,43,20,
        116,217,85,200,9,129,74,44,23,8,42,187,74,233,50,244,
        189,191,90,204,249,158,85,249,95,181,221,222,218,245,
        88,233,171,237,88,128>>},
     {<<186,190,110,128,129,49,7,206,128,220,119,85,54,62,137,
        81,19,55,187,145,79,134,92,232,173,60,3,253,120,240,53,
        192>>,
      <<202,6,50,28,196,88,45,102,43,227,98,231,98,90,153,219,
        37,203,210,186,26,129,3,203,162,28,174,227,248,32,139,
        45,186,190,110,128,129,49,7,206,128,220,119,85,54,62,
        137,81,19,55,187,145,79,134,92,232,173,60,3,253,120,
        240,53,192>>}
    ].

patron_keypair() ->
    #{pubkey := Pub, privkey := Priv} = aecore_env:patron_keypair_for_testing(),
    {Pub, Priv}.

to_bin(B) when is_binary(B) ->
    B;
to_bin(A) when is_atom(A) ->
    atom_to_binary(A, utf8);
to_bin(X) ->
    iolist_to_binary(io_lib:fwrite("~p", [X])).

chain_status(#{<<"chain">> := _} = Res) ->
    Res;
chain_status(Res) ->
    TopHdr = aec_chain:top_header(),
    Height = aec_headers:height(TopHdr),
    Res#{ <<"chain">> => #{ <<"height">> => Height
                          , <<"top_hash">> => encoded_top_hash(TopHdr) }}.

encoded_top_hash(TopHdr) ->
    {ok, TopHash} = aec_headers:hash_header(TopHdr),
    Type = case aec_headers:type(TopHdr) of
               key   -> key_block_hash;
               micro -> micro_block_hash
           end,
    aeser_api_encoder:encode(Type, TopHash).

intersperse(L) ->
    intersperse(L, <<", ">>).

intersperse([H|T], Delim) ->
    [H | [[Delim,X] || X <- T]];
intersperse([], _) ->
    [].

account_balance(K, Balances) ->
    integer_to_binary(proplists:get_value(K, Balances, 0)).

parse_query(Qs) ->
    uri_string:dissect_query(Qs).

account_balances() ->
    {ok, Trees} = aec_chain:get_block_state(aec_chain:top_block_hash()),
    aec_accounts_trees:get_all_accounts_balances(aec_trees:accounts(Trees)).
