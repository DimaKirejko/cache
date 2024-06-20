-module(cache_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    EtsTableManager = #{
        id => ets_table_manager,
        start => {ets_table_manager, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [ets_table_manager]
    },
    Children = [EtsTableManager],
    {ok, {SupFlags, Children}}.