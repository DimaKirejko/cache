-module(cache_client).

%% API
-export([create/1, insert/3, insert/4, lookup/2]).

create(TableName) ->
    cache_srv:create(TableName).

insert(TableName, Key, Value) ->
    cache_srv:insert(TableName, Key, Value).

insert(TableName, Key, Value, TTL) ->
    cache_srv:insert(TableName, Key, Value, TTL).

lookup(TableName, Key) ->
    cache_srv:lookup(TableName, Key).