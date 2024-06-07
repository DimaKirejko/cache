-module(cache_srv).
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([start_link/0,
        create/1,
        insert/3,
        insert/4,
        lookup/2
]).

%% gen_server callbacks
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3
]).

-record(state, {
    table_names :: [atom()]
}).

-record(create, {
    tableName :: atom()
}).
-record(insert_ttl, {
    tableName :: atom(),
    key :: term(),
    value :: term(),
    ttl :: term()
}).
-record(lookup, {
    tableName :: term(),
    key :: term()
}).

%%====================================================================
%% API Functions
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(TableName) ->
    gen_server:call(?MODULE, #create{tableName = TableName}).

insert(TableName, Key, Value) ->
    gen_server:call(?MODULE, #insert_ttl{tableName = TableName, key = Key, value = Value, ttl = permanent}).

insert(TableName, Key, Value, TTL) ->
    gen_server:call(?MODULE, #insert_ttl{tableName = TableName, key = Key, value = Value, ttl = TTL}).

lookup(TableName, Key) ->
    gen_server:call(?MODULE, #lookup{tableName=TableName, key=Key}).

%%====================================================================
%% gen_server Functions
%%====================================================================
init(_Args) ->
    erlang:send_after(60000, self(), delete_obsolete),
    {ok, #state{table_names = []}}.

handle_call(#create{tableName=TableName}, _From, #state{table_names=TableNames}=State) ->
    ets:new(TableName, [named_table, set, public]),
    NewTableNames = [TableName | TableNames],
    io:format("Table ~p successfully created.~n", [TableName]),
    {reply, ok, State#state{table_names = NewTableNames}};

handle_call(#insert_ttl{tableName=TableName, key=Key, value=Value, ttl=TTL}, _From, #state{table_names=TableNames}=State) ->
    case lists:member(TableName, TableNames) of
        true ->
            {Date, Time} = calendar:universal_time(),
            CurrentTimeSeconds = calendar:datetime_to_gregorian_seconds({Date, Time}),
            ExpiryTimeSeconds = check_ETS_no_ttl(CurrentTimeSeconds, TTL),
            true = ets:insert(TableName, {Key, Value, ExpiryTimeSeconds}),
            io:format("Table ~p successfully added ~p with value ~p. Expiry set to ~p.~n", [TableName, Key, Value, ExpiryTimeSeconds]),
            {reply, ok, State};
        false ->
            {reply, {error, table_not_found}, State}
    end;

handle_call(#lookup{tableName=TableName, key=Key}, _From, #state{table_names=TableNames}=State) ->
    case lists:member(TableName, TableNames) of
        true ->
            lookupETS(TableName, Key, State);
        false ->
            {reply, undefined, State}
    end;

handle_call({lookup}, _From, State) ->
    {reply, ok, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(delete_obsolete, #state{table_names=TableNames}=State) ->
    CurrentTimeSeconds = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    lists:foreach(fun(TableName) ->
        MatchSpec = ets:fun2ms(fun({_, _, ExpiryTime}) when ExpiryTime < CurrentTimeSeconds andalso ExpiryTime /= infinity -> true end),
        ets:select_delete(TableName, MatchSpec)
                  end, TableNames),
    erlang:send_after(60000, self(), delete_obsolete),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

lookupETS(TableName, Key, State) ->
    case ets:lookup(TableName, Key) of
        [] ->
            {reply, undefined, State};
        [{_, Value, ExpiryTimeSeconds}] ->
            {Date, Time} = calendar:universal_time(),
            CurrentTimeSeconds = calendar:datetime_to_gregorian_seconds({Date, Time}),
            if CurrentTimeSeconds > ExpiryTimeSeconds ->
                {reply, undefined, State};
                true ->
                    {reply, Value, State}
            end
    end.

check_ETS_no_ttl(CurrentTimeSeconds, TTL) ->
    case TTL of
        permanent ->
            infinity;
        _ ->
            CurrentTimeSeconds + TTL
end.