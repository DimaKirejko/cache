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
    CacheEtsManager = #{
        id => cache_ets_manager,
        start => {cache_ets_manager, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [cache_ets_manager]
    },
    Children = [CacheEtsManager],
    {ok, {SupFlags, Children}}.