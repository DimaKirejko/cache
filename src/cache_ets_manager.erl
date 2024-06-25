-module(cache_ets_manager).
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").

-define(GLOBAL_TABLE_NAME, table).

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

-define(EXPIRE_TIME, 60000).
-define(DELETE_OPERATION, delete_obsolete).

-record(state, {
    table_names :: [atom()]
}).

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server Functions
%%====================================================================

init(_Args) ->
    ets:new(?GLOBAL_TABLE_NAME, [named_table, set, public]),
    io:format("Table ~p successfully created.~n", [?GLOBAL_TABLE_NAME]),
    erlang:send_after(?EXPIRE_TIME, self(), ?DELETE_OPERATION),
    {ok, #state{table_names = [?GLOBAL_TABLE_NAME]}}.

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
        MatchSpec = ets:fun2ms(fun({_, _, ExpiryTime, _}) when ExpiryTime < CurrentTimeSeconds andalso ExpiryTime /= infinity -> true end),
        ets:select_delete(TableName, MatchSpec)
                  end, TableNames).