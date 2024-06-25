-module(cache_client).

%% API
-export([create/1, insert/3, insert/4, lookup/2]).

-spec create(TableName) -> ok  when
    TableName :: atom().
create(TableName) ->
    cache_ets_manager:create(TableName).

-spec insert(TableName, Key, Value) -> ok | {error, table_not_found} when
    TableName :: atom(),
    Key :: atom(),
    Value :: term().
insert(TableName, Key, Value) ->
    insert_with_ttl(TableName, Key, Value, infinity).

-spec insert(TableName, Key, Value, TTL) -> ok | {error, table_not_found} when
    TableName :: atom(),
    Key :: atom(),
    Value :: term(),
    TTL :: pos_integer().
insert(TableName, Key, Value, TTL) ->
    insert_with_ttl(TableName, Key, Value, TTL).

-spec lookup(TableName, Key) -> {error, table_not_found} | undefined | term() when
    TableName :: atom(),
    Key :: atom().
lookup(TableName, Key) ->
    lookup_table_name(TableName, Key).

%% ====================================================================
%% functions
%% ====================================================================

-spec insert_with_ttl(TableName, Key, Value, TTL) -> ok | {error, table_not_found} when
    TableName :: atom(),
    Key :: atom(),
    Value :: term(),
    TTL :: pos_integer() | infinity.
insert_with_ttl(TableName, Key, Value, TTL) ->
    case ets:info(TableName) of
        undefined ->
            {error, table_not_found};
        _ ->
            {Date, Time} = calendar:universal_time(),
            CurrentTimeSeconds = calendar:datetime_to_gregorian_seconds({Date, Time}),
            ExpiryTimeSeconds = get_expiry_time_seconds(CurrentTimeSeconds, TTL),
            true = ets:insert(TableName, {Key, Value, ExpiryTimeSeconds}),
            io:format("Table ~p successfully added ~p with value ~p. Expiry set to ~p.~n",
                [TableName, Key, Value, ExpiryTimeSeconds]),
            ok
    end.

-spec lookup_table_name(TableName, Key) -> {error, table_not_found} | undefined | term() when
    TableName :: atom(),
    Key :: atom().
lookup_table_name(TableName, Key) ->
    case ets:info(TableName) of
        undefined ->
            {error, table_not_found};
        _ ->
            lookup_ets(TableName, Key)
    end.

%%====================================================================
%% Internal
%%====================================================================

get_expiry_time_seconds(CurrentTimeSeconds, TTL) ->
    case TTL of
        infinity ->
            infinity;
        _ ->
            CurrentTimeSeconds + TTL
    end.

lookup_ets(TableName, Key) ->
    case ets:lookup(TableName, Key) of
        [] ->
            undefined;
        [{_, Value, ExpiryTimeSeconds}] ->
            {Date, Time} = calendar:universal_time(),
            CurrentTimeSeconds = calendar:datetime_to_gregorian_seconds({Date, Time}),
            if CurrentTimeSeconds >= ExpiryTimeSeconds ->
                ets:delete(TableName, Key),
                undefined;
            true ->
                Value
            end
    end.
