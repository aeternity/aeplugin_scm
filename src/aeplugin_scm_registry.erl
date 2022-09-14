-module(aeplugin_scm_registry).

-export([ register_session/1
	, register_customer/2
	, register_merchant/4 ]).

-export([ list_merchants/0
        , list_customers/0
        , market_responder_pids/0
	, tags_by_id/1
	, names_by_id/1
	, customers_by_merchant_id/1
	, locate_customer/1
        , locate_merchant/1
        , locate_endpoint/1
        , locate_market_side/1 ]).

-define(MARKET, ae_scm_demo).

-type id() :: aeser_id:id().

register_session(#{opts := #{ role := Role } = Opts, ids := Ids}) ->
    #{responder := R, initiator := I} = Ids,
    case Role of
	responder ->
            gproc:reg({n,l, {?MODULE, market_side, I}}, R),
	    ok;
	initiator ->
            gproc:reg({n,l, {?MODULE, endpoint, I}}, R),
            RegAs = maps:get(register_as, Opts, customer),
            case RegAs of
                customer ->
                    register_customer(I, R);
                #{} = Merch ->
                    Names = maps:get(names, Merch, []),
                    Tags = maps:get(tags, Merch, []),
                    register_merchant(I, R, Names, Tags)
            end
    end.

-spec register_customer(Customer, Market) -> ok
		       when Customer :: id(),
			    Market :: id().
register_customer(Customer, Market) ->
    gproc:reg({n, l, {?MODULE, customer, Customer}}, Market),
    gproc:reg({p, l, {?MODULE, session, Market}}, Customer),
    ok.

-spec register_merchant(id(), id(), [binary()], [binary()]) -> ok.
register_merchant(Id, Market, Names, Tags) when is_list(Names),
                                                is_list(Tags) ->
    case aeser_id:is_id(Id) of
	true ->
	    register_merchant_(Id, Market, Names, Tags);
	false ->
	    error(invalid_id)
    end.

register_merchant_(Merchant, Market, Names, Tags) ->
    gproc:reg({n, l, {?MODULE, merchant, Merchant}}, Market),
    gproc:reg({p, l, {?MODULE, session, Market}}, Merchant),
    %% Names must be unique
    [gproc:reg({n, l, {?MODULE, name, N}}, Merchant) || N <- Names],
    %% Tags are not
    [gproc:reg({p, l, {?MODULE, tag, T}}, Merchant) || T <- Tags],
    ok.

locate_customer(Id) ->
    gproc:where({n,l, {?MODULE, customer, Id}}).

locate_merchant(Id) ->
    gproc:where({n,l, {?MODULE, merchant, Id}}).

locate_endpoint(Id) ->
    gproc:where({n,l, {?MODULE, endpoint, Id}}).

locate_market_side(Id) ->
    gproc:where({n,l, {?MODULE, market_side, Id}}).

%% Note: gproc:select/2 applies a view of the registry as a set of {Key, Pid, Value} tuples.
-spec list_merchants() -> [id()].
list_merchants() ->
    gproc:select(
      {l,n}, [{ {{n,l,{?MODULE, merchant, '$1'}}, '_', '_'}, [], ['$1'] }]).

list_customers() ->
    gproc:select(
      {l,n}, [{ {{n,l,{?MODULE, customer, '$1'}}, '_', '_'}, [], ['$1'] }]).

market_responder_pids() ->
    gproc:select(
      {l,n}, [{ {{n,l,{?MODULE, market_side, '_'}}, '$1', '_'}, [], ['$1'] }]).

tags_by_id(Id) ->
    gproc:select({l,p}, [{ {{p,l,{?MODULE, tag, '$1'}}, '_', Id}, [], ['$1'] }]).


names_by_id(Id) ->
    gproc:select({l,n}, [{ {{n,l,{?MODULE, name, '$1'}}, '_', Id}, [], ['$1'] }]).

customers_by_merchant_id(Id) ->
    gproc:select({l,p}, [{ {{p,l,{?MODULE, session, Id}},'_','$1'}, [], ['$1']}]).
