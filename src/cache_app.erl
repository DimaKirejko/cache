-module(cache_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartTyp, _StartArgs) ->
    cache_sup:start_link().

stop(_State) ->
    ok.
