-module(gen_http_tests).

-include_lib("eunit/include/eunit.hrl").


-compile(export_all).

prepare_cache() ->
  {ok, S} = gen_http:open_socket(),
  gen_http:cache_clear(S),
  {ok, []} = gen_http:cache_list(S),
  {ok, S}.

cache_getset1_test() ->
  {ok, S} = prepare_cache(),
  gen_http:cache_set(S, "url1", "value1"),
  ?assertEqual({ok, [<<"url1">>]}, gen_http:cache_list(S)),
  ?assertEqual(<<"value1">>, gen_http:cache_get(S, "url1")),
  ?assertEqual(<<"value1">>, gen_http:cache_get(S, <<"url1">>)),
  ok.


cache_getset2_test() ->
  {ok, S} = prepare_cache(),
  undefined = gen_http:cache_get(S, "url2"),
  ok.


