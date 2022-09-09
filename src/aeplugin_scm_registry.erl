-module(aeplugin_scm_registry).

-export([ register_session/1
	, register_customer/2
	, register_merchant/3 ]).

-export([ list_merchants/0
	, tags_by_id/1
	, names_by_id/1
	, customers_by_merchant_id/1
	, locate_customer/1 ]).

-define(MARKET, ae_scm_demo).

-type id() :: aeser_id:id().

register_session(#{opts := #{role := Role}, ids := Ids}) ->
    case {Role, Ids} of
	{initiator, _} ->
	    ok;
	{responder, #{responder := R, initiator := I}} ->
	    register_customer(I, R)
    end.

-spec register_customer(Customer, Merchant) -> ok
		       when Customer :: id(),
			    Merchant :: id().
register_customer(Customer, Merchant) ->
    gproc:reg({n, l, {?MODULE, customer, Customer}}, Merchant),
    gproc:reg({p, l, {?MODULE, session, Merchant}}, Customer),
    ok.

-spec register_merchant(id(), [binary()], [binary()]) -> ok.
register_merchant(Id, Names, Tags) when is_list(Names),
					is_list(Tags) ->
    case aeser_id:is_id(Id) of
	true ->
	    register_merchant_(Id, Names, Tags);
	false ->
	    error(invalid_id)
    end.

register_merchant_(Id, Names, Tags) ->
    gproc:reg({n, l, {?MODULE, merchant, Id}}),
    %% Names must be unique
    [gproc:reg({n, l, {?MODULE, name, N}}, Id) || N <- Names],
    %% Tags are not
    [gproc:reg({p, l, {?MODULE, tag, T}}, Id) || T <- Tags],
    ok.

locate_customer(Id) ->    
    gproc:where({n,l, {?MODULE, customer, Id}}).

%% Note: gproc:select/2 applies a view of the registry as a set of {Key, Pid, Value} tuples.
-spec list_merchants() -> [{id(), pid()}].
list_merchants() ->
    gproc:select({l,n}, [{ {{n,l,{?MODULE, merchant, '$1'}}, '_', '_'}, [], ['$1'] }]).

tags_by_id(Id) ->
    gproc:select({l,p}, [{ {{p,l,{?MODULE, tag, '$1'}}, '_', Id}, [], ['$1'] }]).


names_by_id(Id) ->
    gproc:select({l,n}, [{ {{n,l,{?MODULE, name, '$1'}}, '_', Id}, [], ['$1'] }]).

customers_by_merchant_id(Id) ->
    gproc:select({l,p}, [{ {{p,l,{?MODULE, session, Id}},'_','$1'}, [], ['$1']}]).
