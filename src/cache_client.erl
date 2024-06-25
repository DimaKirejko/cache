-module(cache_client).
-include_lib("stdlib/include/ms_transform.hrl").

-define(GLOBAL_TABLE_NAME, table).

%% API
-export([insert/2, insert/3, lookup/1, lookup_by_time_period/2]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec insert(Key, Value) -> ok | {error, table_not_found} when
    Key :: atom(),
    Value :: term().
insert(Key, Value) ->
    insert_with_ttl(?GLOBAL_TABLE_NAME, Key, Value, infinity).

-spec insert(Key, Value, TTL) -> ok | {error, table_not_found} when
    Key :: atom(),
    Value :: term(),
    TTL :: pos_integer().
insert(Key, Value, TTL) ->
    insert_with_ttl(?GLOBAL_TABLE_NAME, Key, Value, TTL).

-spec lookup(Key) -> {error, table_not_found} | undefined | term() when
    Key :: atom().
lookup(Key) ->
    lookup_table_name(?GLOBAL_TABLE_NAME, Key).

-spec lookup_by_time_period(DataFrom, DataTo) -> undefined | term() when
    DataFrom :: binary(),
    DataTo :: binary().
lookup_by_time_period(DataFrom, DataTo) ->
    GregorianDataFrom = convert_to_gregorian_seconds(DataFrom),
    GregorianDataTo = convert_to_gregorian_seconds(DataTo),
    find_tables_in_interval(?GLOBAL_TABLE_NAME, GregorianDataFrom, GregorianDataTo).

%%====================================================================
%% Internal
%%====================================================================

%%insert
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
            true = ets:insert(TableName, {Key, Value, ExpiryTimeSeconds, CurrentTimeSeconds}),
            io:format("Table ~p successfully added ~p with value ~p. Expiry set to ~p. time of creation: ~p.~n",
                [TableName, Key, Value, ExpiryTimeSeconds, CurrentTimeSeconds]),
            ok
    end.

get_expiry_time_seconds(CurrentTimeSeconds, TTL) ->
    case TTL of
        infinity ->
            infinity;
        _ ->
            CurrentTimeSeconds + TTL
    end.

%%lookup
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

lookup_ets(TableName, Key) ->
    case ets:lookup(TableName, Key) of
        [] ->
            undefined;
        [{_, Value, ExpiryTimeSeconds, _}] ->
            {Date, Time} = calendar:universal_time(),
            CurrentTimeSeconds = calendar:datetime_to_gregorian_seconds({Date, Time}),
            if CurrentTimeSeconds >= ExpiryTimeSeconds ->
                ets:delete(TableName, Key),
                undefined;
            true ->
                Value
            end
    end.

%%lookup_by_date
convert_to_gregorian_seconds(DateTimeStr) ->
    [Y, M, D, H, Min, S] = [list_to_integer(Part) || Part <- re:split(DateTimeStr, "[/ :]", [{return, list}])],
    calendar:datetime_to_gregorian_seconds({{Y, M, D}, {H, Min, S}}).

find_tables_in_interval(Tab, GregorianDataFrom, GregorianDataTo) ->
    MatchSpec = ets:fun2ms(fun({_, _, _, TimeCreated} = Tuple)
        when TimeCreated >= GregorianDataFrom,
        TimeCreated =< GregorianDataTo -> Tuple end),
    case ets:select(Tab, MatchSpec) of
        [] ->
            io:format("Data not found ~n"),
            undefined;
        Records ->
            io:format("Data ~p found ~n", [Records]),
            extract_and_decorate_values(Records, [])
    end.

extract_and_decorate_values([], Acc) ->
    Acc;
extract_and_decorate_values([{Key, Value, ExpiryTimeSeconds, _CurrentTimeSeconds} | Tail], Acc) ->
    {Date, Time} = calendar:universal_time(),
    CurrentTimeSeconds = calendar:datetime_to_gregorian_seconds({Date, Time}),
    if CurrentTimeSeconds >= ExpiryTimeSeconds ->
        ets:delete(?GLOBAL_TABLE_NAME, Key),
        extract_and_decorate_values(Tail, [{Key, Value} | Acc]);
    true ->
            extract_and_decorate_values(Tail, [{Key, Value} | Acc])
    end.