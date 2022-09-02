%% -*- erlang-indent-mode: 4; indent-tabs-mode: nil -*-
-module(aeplugin_scm_sup).
-behavior(supervisor).

-export([start_link/0,
         init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{ strategy  => one_for_one
                , intensity => 3
                , period    => 60 },
    Server = aeplugin_scm_server,
    {ok, {SupFlags,
          [
           #{ id       => Server
            , start    => {Server, start_link, []}
            , restart  => permanent
            , shutdown => 2000
            , type     => worker
            , modules  => [Server] }
          ]}}.
