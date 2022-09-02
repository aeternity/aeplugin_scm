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

-record(st, {}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    {ok, #st{}}.

handle_call(_Req, _From, St) ->
    {reply, unknown_call, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.
