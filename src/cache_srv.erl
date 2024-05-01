-module(cache_srv).
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([start_link/0,
        create/1,
        insert/3,
        insert/4,
        lookup/2
        %delete_obsolete/0 %%можливо потрібно буде заекспортувати
]).

%% gen_server callbacks
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

%%====================================================================
%% API Functions
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(TableName) ->
    gen_server:call(?MODULE, {create, TableName}).

insert(TableName, Key, Value) ->
    gen_server:call(?MODULE, {insert_ttl, TableName, Key, Value, 60}).

insert(TableName, Key, Value, TTL) ->
    gen_server:call(?MODULE, {insert_ttl, TableName, Key, Value, TTL}).

lookup(TableName, Key) ->
    gen_server:call(?MODULE, {lookup, TableName, Key}).

%%====================================================================
%% gen_server Functions
%%====================================================================
init(_Args) ->
    erlang:send_after(1000, self(), delete_obsolete),
    {ok, #{}}.

handle_call({create, TableName}, _From, State) ->
    TableId = ets:new(TableName, [named_table, set, public]),
    NewState = State#{TableName => TableId}, % Store TableId in the state map
    io:format("Table ~p successfully created with ID ~p.~n", [TableName, TableId]),
    {reply, ok, NewState};

handle_call({insert_ttl, TableName, Key, Value, TTL}, _From, State) ->
    TableId = maps:get(TableName, State),
    {Date, Time} = calendar:universal_time(),
    CurrentTimeSeconds = calendar:datetime_to_gregorian_seconds({Date, Time}),
    ExpiryTimeSeconds = CurrentTimeSeconds + TTL,
    true = ets:insert(TableId, {Key, Value, ExpiryTimeSeconds}),
    io:format("Table ~p successfully added ~p and ~p.~n", [TableName, Key, Value]),
    {reply, ok, State};

handle_call({lookup, TableName, Key}, _From, State) ->
    case maps:find(TableName, State) of
        {ok, TableId} ->
            case ets:lookup(TableId, Key) of
                [] ->
                    {reply, undefined, State};
                [{_, Value, _TTL}] ->
                    {reply, Value, State}
            end;
        error ->
            {reply, undefined, State}
    end;

handle_call({lookup}, _From, State) ->
    {reply, ok, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(delete_obsolete, State) ->
    {Date, Time} = calendar:universal_time(),
    CurrentTimeSeconds = calendar:datetime_to_gregorian_seconds({Date, Time}),
    %% формуємо match-spec
    MatchSpecFun = fun(_TableName, TableId) ->
        MatchSpec = ets:fun2ms(fun({_, _, Number}) when Number < CurrentTimeSeconds -> true end),
        ets:select_delete(TableId, MatchSpec)
                   end,
    %% використовуємо match-spec
    maps:map(MatchSpecFun, State),
    erlang:send_after(1000, self(), delete_obsolete),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================
