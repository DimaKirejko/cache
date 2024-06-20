-module(cache_cleaner).
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    create/1
]).

-define(EXPIRE_TIME, 6000).
-define(DELETE_OPERATION, delete_obsolete).

-record(state, {
    table_names :: [atom()]
}).

-record(create, {
    tableName :: atom()
}).

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(TableName) ->
    gen_server:call(?MODULE, #create{tableName = TableName}).

%%====================================================================
%% gen_server Functions
%%====================================================================

init(_Args) ->
    erlang:send_after(?EXPIRE_TIME, self(), ?DELETE_OPERATION),
    {ok, #state{table_names = []}}.

handle_call(#create{tableName = TableName}, _From, #state{table_names = TableNames} = State) ->
    ets:new(TableName, [named_table, set, public]),
    NewTableNames = [TableName | TableNames],
    io:format("Table ~p successfully created (TEST#122).~n", [TableName]),
    {reply, ok, State#state{table_names = NewTableNames}};

handle_call(_Request, _From, State) ->
    io:format("Got unexpected call"),
    {reply, ok, State}.

handle_cast(_Cast, State) ->
    io:format("Got unexpected cast"),
    {noreply, State}.

handle_info(delete_obsolete, #state{table_names = TableNames} = State) ->
    perform_cleanup(TableNames),
    erlang:send_after(?EXPIRE_TIME, self(), ?DELETE_OPERATION),
    {noreply, State};

handle_info(Info, State) ->
    io:format("Got unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

perform_cleanup(TableNames) ->
    CurrentTimeSeconds = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    lists:foreach(fun(TableName) ->
        MatchSpec = ets:fun2ms(fun({_, _, ExpiryTime}) when ExpiryTime < CurrentTimeSeconds andalso ExpiryTime /= infinity -> true end),
        ets:select_delete(TableName, MatchSpec)
                  end, TableNames).
