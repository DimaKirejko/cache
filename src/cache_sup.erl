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
    CacheServer = #{
        id => cache_srv,
        start => {cache_srv, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [cache_srv]
    },
    Children = [CacheServer],
    {ok, {SupFlags, Children}}.