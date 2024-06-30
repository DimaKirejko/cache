-module(cache_app_SUITE).

-include_lib("/usr/local/Cellar/erlang//26.0.2/lib/erlang/lib/common_test-1.25/include/ct.hrl").
-include_lib("/usr/local/Cellar/erlang//26.0.2/lib/erlang/lib/stdlib-5.0.2/include/assert.hrl").

-export([all/0, suite/0]).

-export([
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    test_delete_obsolete/1,
    test_ttl/1,
    test_lookup/1
]).

suite() ->
    [{timetrap, {seconds, 5}}].

all() ->
    [test_delete_obsolete, test_ttl, test_lookup].

init_per_testcase(_TestCase, _Config) ->
    cache_ets_manager:start_link_test(3000),
    [].

end_per_testcase(_TestCase, _Config) ->
    ok.

test_delete_obsolete(_Config) ->
    cache_ets_manager:create(test_table),
    cache_client:insert(test_table, "key1", "value1", 1),
    timer:sleep(4000),
    ?assertEqual([], ets:lookup(test_table, "key1")). %% use ets functions, bypassing the API functions of the application

test_ttl(_Config) ->
    cache_ets_manager:create(test_table2),
    cache_client:insert(test_table2, "key2", "value2", 1),
    timer:sleep(2000),
    ?assertEqual(undefined, cache_client:lookup(test_table2, "key2")).

test_lookup(_Config) ->
    cache_ets_manager:create(test_table2),
    cache_client:insert(test_table2, "key2", "value2"),
    ?assertEqual("value2", cache_client:lookup(test_table2, "key2")).
