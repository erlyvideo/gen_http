#!/usr/bin/env escript
%%! -pa ebin  -smp enable +K true +A 16 +a 2048
-mode(compile).

main([]) ->
  main(["8081"]);

main([Port]) ->
  {ok, Listen} = gen_http:listen(list_to_integer(Port), [{backlog,1000}]),
  listen_loop(Listen).

listen_loop(Listen) ->
  gen_http:accept_once(Listen),
  receive
    {http_connection, Listen, Socket} ->
      io:format("Accepting new connection~n"),
      Pid = spawn(fun() ->
        handle_client()
      end),
      gen_http:controlling_process(Socket, Pid),
      Pid ! {socket, Socket},
      listen_loop(Listen);
    Else ->
      io:format("Listener: ~p~n", [Else]),
      erlang:exit({listener,Else})
  end.


handle_client() ->
  receive
    {socket, Socket} -> handler_loop(Socket)
  end.

handler_loop(Socket) ->
  gen_http:active_once(Socket),
  receive
    {http, Socket, _Method, _URL, _Keepalive, _Version, _Headers} = Req ->
      io:format("Request: ~p~n", [Req]),
      % gen_http:flush_body(Socket),
      gen_http:send(Socket, "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nGood\n"),
      handler_loop(Socket);
    {http_closed, Socket} ->
      io:format("Handler closed~n");
    % {http, Socket, empty} ->
    %   handler_loop(Socket);
    Else ->
      io:format("Message: ~p~n", [Else]),
      erlang:exit({handler,Else})
  end.

  