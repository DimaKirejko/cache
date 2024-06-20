-module(cache_client).

%% API
-export([create/1, insert/3, insert/4, lookup/2]).

create(TableName) ->
    cache_cleaner:create(TableName).

insert(TableName, Key, Value) ->
    insert_with_ttl(TableName, Key, Value, infinity).

insert(TableName, Key, Value, TTL) ->
    insert_with_ttl(TableName, Key, Value, TTL).

lookup(TableName, Key) ->
    lookup_table_name(TableName, Key).

%% ====================================================================
%% functions
%% ====================================================================

-spec insert_with_ttl(TableName, Key, Value, TTL) -> ok | {error, table_not_found} when
    TableName :: atom(),
    Key :: term(),
    Value :: term(),
    TTL :: pos_integer() | infinity.
insert_with_ttl(TableName, Key, Value, TTL) ->
    case ets:info(TableName) of
        undefined ->
            {error, table_not_found};
        _ ->
            {Date, Time} = calendar:universal_time(),
            CurrentTimeSeconds = calendar:datetime_to_gregorian_seconds({Date, Time}),
            ExpiryTimeSeconds = check_ets_no_ttl(CurrentTimeSeconds, TTL),
            true = ets:insert(TableName, {Key, Value, ExpiryTimeSeconds}),
            io:format("Table ~p successfully added ~p with value ~p. Expiry set to ~p.~n",
                [TableName, Key, Value, ExpiryTimeSeconds]),
            ok
    end.

-spec lookup_table_name(TableName, Key) -> {error, table_not_found} | undefined | term() when
    TableName :: atom(),
    Key :: term().
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

check_ets_no_ttl(CurrentTimeSeconds, TTL) ->
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
            if CurrentTimeSeconds > ExpiryTimeSeconds ->
                ets:delete(TableName, Key),
                undefined;
                true ->
                    Value
            end
    end.


