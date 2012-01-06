#!/usr/bin/env escript
%%! -pa ebin

-include_lib("inets/include/httpd.hrl").
-compile(export_all).

-define(D(X), io:format("~p: ~p~n", [?LINE, X])).


main([]) ->
  inets:start(),
  {ok, Pid} = inets:start(httpd, [{port,9000},{server_name,"aa"},{server_root,"."},{document_root,"."}]),
  ?D(started_server),
  % {ok, Sock} = gen_http:connect("localhost", 9000),
  Host = "192.168.0.8",
  Port = 8080,
  ?D(connected),
  % Path = "/client.erl",
  Path = "/dvb/2/manifest.f4m",
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
  
  Pids = [spawn(fun() -> make_req(undefined) end) || _ <- lists:seq(1,100)],
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
  
make_req(undefined) ->
  {Host, Port, Req} = ets:lookup_element(cache, req, 2),
  {ok, Sock} = gen_http:connect(Host, Port),
  put(req, Req),
  make_req(Sock);
  

make_req(Sock) ->
  gen_http:send(Sock, get(req)),
  gen_http:active_once(Sock),
  receive
    {http, Sock, _Code, Keepalive, _Version, _Headers} ->
      % ?D({reply, _Code, _Headers}),
      ok = gen_http:flush_body(Sock),
      % receive_body(Sock, []),
      Count = ets:update_counter(cache, counter, 1),
      if
        Count rem 100 == 0 -> 
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
      make_req(Sock);
    {http_closed, Sock} ->
      ?D({closed}),
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
    {http, Sock, Bin} -> receive_body(Sock, [Bin|Acc]);
    {http_error, Sock, Error} -> {error, Error};
    {http_closed, Sock} -> {error, closed}
  end.


