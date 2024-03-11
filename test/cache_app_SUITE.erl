-module(cache_app_SUITE).

-include_lib("/usr/local/Cellar/erlang//26.0.2/lib/erlang/lib/common_test-1.25/include/ct.hrl").
-include_lib("/usr/local/Cellar/erlang//26.0.2/lib/erlang/lib/stdlib-5.0.2/include/assert.hrl").

-export([all/0, suite/0]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([test_ttl/1, test_lookup/1]).

suite() ->
    [{timetrap, {seconds, 5}}].

all() ->
    [test_ttl, test_lookup].

init_per_testcase(_TestCase, _Config) ->
    application:ensure_all_started(cache),
    [].

end_per_testcase(_TestCase, _Config) ->
    application:stop(cache),
    ok.

test_ttl(_Config) ->
    cache_app:create(test_table),
    cache_app:insert(test_table, "key2", "value2", 1),
    timer:sleep(1500),
    undefined = cache_app:lookup(test_table, "key2").

test_lookup(_Config) ->
    cache_app:create(test_table2),
    cache_app:insert(test_table2, "key2", "value2"),
    "value2" = cache_app:lookup(test_table2, "key2").