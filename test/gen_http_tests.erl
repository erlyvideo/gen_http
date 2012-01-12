-module(gen_http_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/log.hrl").

-define(PORT, 23456).

-compile(export_all).

prepare_cache() ->
  {ok, S} = gen_http:open_socket(),
  gen_http:cache_clear(S),
  [] = gen_http:cache_list(S),
  {ok, S}.

cache_getset1_test() ->
  {ok, S} = prepare_cache(),
  gen_http:cache_set(S, "url1", "value1"),
  ?assertEqual([<<"url1">>], gen_http:cache_list(S)),
  ?assertEqual(<<"value1">>, gen_http:cache_get(S, "url1")),
  ?assertEqual(<<"value1">>, gen_http:cache_get(S, <<"url1">>)),
  ok.


cache_getset2_test() ->
  {ok, S} = prepare_cache(),
  undefined = gen_http:cache_get(S, "url2"),
  ok.



multiacceptor1(Parent, L, N) ->
  gen_http:accept_once(L),
  receive
    {http_connection, L, Sock1} ->
      gen_http:controlling_process(Sock1, Parent),
      Parent ! {sock,N,Sock1};
    stop ->
      Parent ! {refused, N},
      ok  
  after
    4000 -> erlang:exit({timeout,N})
  end.

pseudocall() ->
  {ok, S} = gen_tcp:connect("localhost", ?PORT, []),
  ok = gen_tcp:send(S, "GET / HTTP/1.1\r\nConnection: close\r\n\r\n"),
  {ok, S}.



multiaccept_test_() ->
  {spawn, fun() ->
    {ok, L} = gen_http:listen(?PORT, [{backlog,2}]),
    Self = self(),
    _Pid1 = spawn_link(fun() -> multiacceptor1(Self, L, 1) end),
    Pid2 = spawn_link(fun() -> multiacceptor1(Self, L, 2) end),
    _Pid3 = spawn_link(fun() -> multiacceptor1(Self, L, 3) end),
    Pid2 ! stop,
    ?assertMatch({ok, _}, pseudocall()),
    ?assertMatch({ok, _}, pseudocall()),
    receive {sock,1,_} -> ok after 1000 -> erlang:exit({timeout,parent,1}) end,
    receive {refused,2} -> ok after 1000 -> erlang:exit({timeout,parent,2}) end,
    receive {sock,3,_} -> ok after 1000 -> erlang:exit({timeout,parent,3}) end,

    % Now check backlog
    ?assertMatch({ok, _}, gen_tcp:connect("localhost", ?PORT, [], 300)),
    ?assertMatch({ok, _}, gen_tcp:connect("localhost", ?PORT, [], 300)),
    ?assertEqual({error, timeout}, gen_tcp:connect("localhost", ?PORT, [], 300)),

    ok
  end}.

-define(CMD_GET_EXHAUSTED, 13).


blocking_send_test_() ->
  {spawn, fun() ->
  {ok, L} = gen_http:listen(?PORT),
  Pid1 = spawn_link(fun() ->
    {ok, S} = pseudocall(),
    receive recv -> ok after 3000 -> erlang:exit({timeout,child,1}) end,
    gen_tcp:recv(S, 1024*1024),
    receive recv2 -> ok after 3000 -> erlang:exit({timeout,child,2}) end,
    ok
  end),

  Self = self(),
  spawn_link(fun() -> multiacceptor1(Self, L, 1) end),
  
  %% First check 'empty' message
  Sock = receive {sock, 1, S} -> S after 1000 -> erlang:exit({timeout,parent,1}) end,
  gen_http:send_async(Sock, crypto:rand_bytes(1024*1024)),
  ?assertEqual("wait", port_control(Sock, ?CMD_GET_EXHAUSTED, <<>>)),
  Pid1 ! recv,
  receive
    {http, Sock, empty} -> ok
  after
    1000 -> erlang:exit({timeout,send_async})
  end,
  
  
  %% Now check blocking port
  
  true = erlang:port_command(Sock, crypto:rand_bytes(3*1024*1024), [nosuspend]),
  false = erlang:port_command(Sock, crypto:rand_bytes(3*1024*1024), [nosuspend]),
  Pid1 ! recv2
  
  end}.

  
