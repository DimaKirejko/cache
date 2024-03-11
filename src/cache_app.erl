-module(cache_app).
-behaviour(application).

-export([start/2, stop/1]).

-export([create/1, insert/3, insert/4, lookup/2]).

start(_StartTyp, _StartArgs) ->
    cache_sup:start_link().

stop(_State) ->
    ok.

%%====================================================================
%% application interface
%%====================================================================

create(TableName) ->
    cache_srv:create(TableName).

insert(TableName, Key, Value) ->
    cache_srv:insert(TableName, Key, Value).

insert(TableName, Key, Value, TTL) ->
    cache_srv:insert(TableName, Key, Value, TTL).

lookup(TableName, Key) ->
    cache_srv:lookup(TableName, Key).