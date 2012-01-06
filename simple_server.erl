#!/usr/bin/env escript
%%! -pa ebin  -smp enable +K true +A 16 +a 2048


main([]) ->
  main(["9000"]);
  
main([Port]) ->
  {ok, Listen} = gen_http:listen(list_to_integer(Port), [{backlog,1000}]),
  listen_loop(Listen).

listen_loop(Listen) ->
  gen_http:accept_once(Listen),
  receive
    {http_connection, Listen, Socket} ->
      Pid = spawn(fun() ->
        handle_client()
      end),
      gen_http:controlling_process(Socket, Pid),
      Pid ! {socket, Socket},
      listen_loop(Listen);
    Else ->
      erlang:exit({listener,Else})
  end.


handle_client() ->
  receive
    {socket, Socket} -> handler_loop(Socket)
  end.

handler_loop(Socket) ->
  gen_http:active_once(Socket),
  receive
    {http, Socket, Method, URL, Keepalive, Version, Headers} = Req ->
      io:format("Request: ~p~n", [Req]),
      % gen_http:flush_body(Socket),
      gen_http:send(Socket, "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nGood\n"),
      handler_loop(Socket);
    {http_closed, Socket} ->
      io:format("Handler closed~n");
    Else ->
      erlang:exit({handler,Else})
  end.