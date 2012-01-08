-module(http_bench_client).

-compile(export_all).

-define(D(X), io:format("~p: ~p~n", [?LINE, X])).


run(URL) ->
  % inets:start(),
  % {ok, Pid} = inets:start(httpd, [{port,9000},{server_name,"aa"},{server_root,"."},{document_root,"."}]),
  % ?D(started_server),
  % {ok, Sock} = gen_http:connect("localhost", 9000),
  {ok, {http,[],Host, Port, Path, []}} = http_uri:parse(URL),
  % ?D(connected),
  Req = iolist_to_binary([
    "GET ", Path ," HTTP/1.1\r\n",
    "Host: localhost\r\n",
    "Connection: Keep-Alive\r\n",
    "\r\n"
  ]),
  ets:new(cache, [public,named_table]),
  ets:insert(cache, {counter, 0}),
  ets:insert(cache, {start_time, erlang:now()}),
  ets:insert(cache, {req, {Host, Port, Req}}),
  % ?D(sent),
  
  io:format("~10.s ~10.s ~10.s ~10.s ~10.s ~10.s~n", [count, total, send, header, body, update]),
  
  Pids = [spawn(fun() -> make_req(undefined) end) || _ <- lists:seq(1,10)],
  [erlang:monitor(process, Pid) || Pid <- Pids],
  [receive
    {'DOWN', _, _, Pid, _} -> ok
  end || Pid <- Pids],
  % make_req(undefined),
  % gen_http:active_once(Sock),
  % receive
  %   A -> ?D(A)
  % end,
  % Body = receive_body(Sock, []),
  % ?D(Body),
  ok.

  

-define(TD(X1,X2), timer:now_diff(X1,X2)).
  
make_req(undefined) ->
  {Host, Port, Req} = ets:lookup_element(cache, req, 2),
  ?D({reconnecting,self(), Host, Port}),
  {ok, Sock} = gen_http:connect(Host, Port),
  gen_http:setopts(Sock, [{chunk_size,1024*1024}]),
  put(req, Req),
  make_req(Sock);
  

make_req(Sock) ->
  T1 = erlang:now(),
  gen_http:send(Sock, get(req)),
  T2 = erlang:now(),
  gen_http:active_once(Sock),
  receive
    {http, Sock, _Code, Keepalive, _Version, _Headers} ->
      T3 = erlang:now(),
      % ?D({reply, _Code, _Headers}),
      % ok = gen_http:flush_body(Sock),
      T4 = erlang:now(),
      % ?D(_Headers),
      receive_body(Sock, []),
      % erlang:halt(0),
      Count = ets:update_counter(cache, counter, 1),
      % ?D({Count,proplists:get_value('Content-Length', _Headers)}),
      if
        Count rem 10000 == 0 -> 
          Now = erlang:now(),
          Start = ets:lookup_element(cache, start_time, 2), 
          Delta = timer:now_diff(Now,Start) div 1000000,
          if Delta > 0 ->
            ?D({Count, Count div Delta});
          true ->
            ?D(Count)
          end;  
        true -> ok
      end,
      T5 = erlang:now(),
      case get(debug) of
        true -> io:format("~10.. B ~10.. B ~10.. B ~10.. B ~10.. B ~10.. B~n", [Count, ?TD(T5,T1),?TD(T2,T1),?TD(T3,T2),?TD(T4,T3),?TD(T5,T4)]);
        _ -> ok
      end,
      case Keepalive of
        keepalive -> make_req(Sock);
        close -> gen_http:close(Sock), make_req(undefined)
      end;
    {http_closed, Sock} ->
      make_req(undefined);
    {http_error, Sock, Error} ->
      ?D({Error}),
      make_req(undefined);
    Else ->
      ?D(Else)  
  end.


receive_body(Sock, Acc) ->
  gen_http:active_once(Sock),
  receive
    {http, Sock, eof} -> lists:reverse(Acc);
    {http, Sock, Bin} -> receive_body(Sock, [Bin]);
    {http_error, Sock, Error} -> {error, Error};
    {http_closed, Sock} -> {error, closed}
  end.
