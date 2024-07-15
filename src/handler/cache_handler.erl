-module(cache_handler).
-export([init/2]).

-behavior(cowboy_handler).

-define(RESULT_OK, [{<<"resultt">>, <<"ok">>}]).
-define(RESULT_TABLE_NOT_FOUND, [{<<"result">>, <<"table_not_found">>}]).
-define(RESULT_NOT_FOUND, [{<<"result">>, <<"not found">>}]).

init(Req0=#{method := <<"POST">>}, State) ->
    {ok, Json, _R} = cowboy_req:read_body(Req0),
    InformationFromJson = jsx:decode(Json, [return_maps]),
    process_request(InformationFromJson, Req0, State);

init(Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"allow">> => <<"POST">>
    }, Req0),
    {ok, Req, State}.

process_request(InformationFromJson, Req0, State) ->
    Response = request_handler(InformationFromJson),
    EncodeResponse = jsx:encode(Response),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, EncodeResponse, Req0),
    {ok, Req, State}.

request_handler(#{<<"action">> := <<"insert">>, <<"key">> := Key, <<"value">> := Value, <<"ttl">> := TTL}) ->
    case cache_client:insert(Key, Value, TTL) of
        ok ->
            ?RESULT_OK;
        {error, table_not_found} ->
            ?RESULT_TABLE_NOT_FOUND
        end;

request_handler(#{<<"action">> := <<"insert">>, <<"key">> := Key, <<"value">> := Value}) ->
   case cache_client:insert(Key, Value) of
       ok ->
           ?RESULT_OK;
       {error, table_not_found} ->
           ?RESULT_TABLE_NOT_FOUND
       end;

request_handler(#{<<"action">> := <<"lookup">>, <<"key">> := Key}) ->
    case cache_client:lookup(Key) of
        undefined ->
            ?RESULT_NOT_FOUND;
        Value ->
            [{<<"result">>, Value}]
    end;

request_handler(#{<<"action">> := <<"lookup_by_date">>, <<"date_from">> := DataFrom, <<"date_to">> := DataTo}) ->
    case cache_client:lookup_by_time_period(DataFrom, DataTo) of
        undefined ->
            ?RESULT_NOT_FOUND;
        Value ->
            [{<<"result">>, Value}]
    end.
