-module(cache_app).
-behaviour(application).

-export([start/2, stop/1]).

-define(TABLE, table).

start(_StartTyp, _StartArgs) ->
Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/cache_server", cache_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(cache_handler, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    cache_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(cache_handler).
