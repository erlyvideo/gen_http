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


raw_mode_test_() ->
  {spawn, fun() ->
    {ok, L} = gen_http:listen(?PORT),
    Self = self(),
    
    _Pid1 = spawn_link(fun() ->
      {ok, S} = gen_tcp:connect("localhost", ?PORT, [binary,{active,false}]),
      ok = gen_tcp:send(S, "GET / HTTP/1.1\r\nConnection: keep-alive, Upgrade\r\nUpgrade: websocket\r\n\r\nHi!\n"),
      ?assertEqual({ok, <<"Bye\n">>}, gen_tcp:recv(S, 4)),
      ok = gen_tcp:send(S, "Msg2\n"),
      ?assertEqual({ok, <<"Reply2\n">>}, gen_tcp:recv(S, 7)),
      Self ! ok
    end),
    
    Self = self(),
    ?D(h0),
    spawn_link(fun() -> multiacceptor1(Self, L, 1) end),

    %% First check 'empty' message
    Sock = receive {sock, 1, S} -> S after 1000 -> erlang:exit({timeout,parent,1}) end,
    gen_http:active_once(Sock),
    receive 
      {tcp, Sock, <<"Hi!\n">>} -> ok
    after
      1000 -> erlang:exit({invalid,client})
    end,
    gen_http:send(Sock, "Bye\n"),
    gen_http:setopts(Sock, [{active,once}]),
    receive
      {tcp, Sock, <<"Msg2\n">>} -> ok
    after
      1000 -> erlang:exit({timeout,parent,2})
    end,
    gen_http:send(Sock, "Reply2\n"),
    gen_http:send_async(Sock, crypto:rand_bytes(1024*1024)),
    receive ok -> ok after 1000 -> erlang:exit({timeout,child}) end,
    ok
  end}.
  

catch_request(Port, Fun) ->
  Self = self(),
  Pid = spawn_link(fun() ->
    {ok, L} = gen_http:listen(Port),
    {ok, S} = gen_http:accept(L, 1000),
    gen_http:active_once(S),
    receive
      Response -> Self ! {resp, self(), Response}
    end
  end),
  _Pid2 = spawn_link(Fun),
  receive
    {resp, Pid, Response} -> Response
  after
    1000 -> erlang:exit(timeout)  
  end.
  
test_request_helper(Request) ->
  catch_request(?PORT, fun() ->
    {ok, S} = gen_tcp:connect("localhost", ?PORT, [binary]),
    gen_tcp:send(S, Request)
  end).

proto_parsing_test_() ->
  {spawn, fun() ->
    ?assertMatch({http, _, 'GET', <<"/">>, _, _, []}, test_request_helper("GET / HTTP/1.0\r\n\r\n"))
    ,?assertMatch({rtsp, _, 'PLAY', <<"/">>, _, _, []}, test_request_helper("PLAY / RTSP/1.0\r\n\r\n"))
    ,?assertMatch({rtsp, _, 'PAUSE', <<"/">>, _, _, []}, test_request_helper("PAUSE / RTSP/1.0\r\n\r\n"))
    ,?assertMatch({rtsp, _, 'SETUP', <<"/">>, _, _, []}, test_request_helper("SETUP / RTSP/1.0\r\n\r\n"))
    ,?assertMatch({rtsp, _, 'RECORD', <<"/">>, _, _, []}, test_request_helper("RECORD / RTSP/1.0\r\n\r\n"))
    ,?assertMatch({rtsp, _, 'ANNOUNCE', <<"/">>, _, _, []}, test_request_helper("ANNOUNCE / RTSP/1.0\r\n\r\n"))
    ,?assertMatch({rtsp, _, 'DESCRIBE', <<"/">>, _, _, []}, test_request_helper("DESCRIBE / RTSP/1.0\r\n\r\n"))
    ,?assertMatch({rtsp, _, 'TEARDOWN', <<"/">>, _, _, []}, test_request_helper("TEARDOWN / RTSP/1.0\r\n\r\n"))
    ,?assertMatch({rtsp, _, 'GET_PARAMETER', <<"/">>, _, _, []}, test_request_helper("GET_PARAMETER / RTSP/1.0\r\n\r\n"))
    ,?assertMatch({rtsp, _, 'SET_PARAMETER', <<"/">>, _, _, []}, test_request_helper("SET_PARAMETER / RTSP/1.0\r\n\r\n"))
  end}.
