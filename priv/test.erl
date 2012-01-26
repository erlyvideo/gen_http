#!/usr/bin/env escript
%%! -pa ebin  -smp enable +K true +A 16 +a 2048

-define(C(X), io:format("client:~p ~p~n", [?LINE,X])).
-define(S(X), io:format("server:~p ~p~n", [?LINE,X])).

main(Args) ->
  inets:start(),
  Opts1 = [binary, {packet, raw}, {reuseaddr, true}, 
          {keepalive, true}, {backlog, 4000}, {active, once}],
  {ok, Listen} = gen_http:listen(9000, Opts1),
  
  ets:new(http_cache, [set,named_table,public]),
  Bin = ["Hello World!\n" || _ <- lists:seq(1,2)],
  ets:insert(http_cache, {<<"/index.html">>, reply(Bin, keepalive)}),
  ets:insert(http_cache, {counter, 0}),
  
  [spawn(fun() -> 
    gen_http:accept_once(Listen), 
    timer:sleep(550),
    Timeout = 10000,
    receive
      {http_connection, Listen, _Sock} -> ?S(accepted);
      {http_closed, Listen} -> ?S(closed)
    after
      Timeout -> ?S(timeout)
    end
  end) || _ <- lists:seq(1,40)],
  
  Listener1 = spawn(fun() ->
    listen(Listen)
  end),
  erlang:monitor(process, Listener1),

  Listener2 = spawn(fun() ->
    listen(Listen)
  end),
  erlang:monitor(process, Listener2),
  
  
  timer:sleep(500),
  Client = spawn(fun() ->
    connect(9000)
  end),
  erlang:monitor(process, Client),
  
  timer:sleep(1000),
  
  receive
    {'DOWN', _, process, Client, Reason1} -> io:format("client died ~p~n", [Reason1])
  end,
  Listener1 ! stop,
  Listener2 ! stop,
  
  receive
    {'DOWN', _, process, Listener1, Reason2} -> io:format("listener1 died ~p~n", [Reason2])
  end,
  receive
    {'DOWN', _, process, Listener2, Reason3} -> io:format("listener1 died ~p~n", [Reason3])
  end,
  
  timer:sleep(500),
  gen_http:close(Listen),
  ok.

-define(SIZE, 100000).

keepalive(keepalive) -> "Keep-Alive";
keepalive(close) -> "Close".

reply(Socket, Reply, Keepalive) ->
  gen_http:send(Socket, reply(Reply, Keepalive)).

reply(Reply, Keepalive) ->
  iolist_to_binary([
    "HTTP/1.1 200 OK\r\n",
    "Connection: ",keepalive(Keepalive), "\r\n",
    io_lib:format("Content-Length: ~p\r\n", [iolist_size(Reply)]),
    "\r\n",
    Reply
  ]).

listen(Listen) ->
  ?S({open_port,Listen}),
  % Bin = crypto:rand_bytes(?SIZE),
  timer:send_interval(1000, dump),
  
  put(prev_request_count, 0),
  put(clients, 0),
  % Spawners = [spawn_link(fun() ->
  %   listen(Listen)
  % end) || _N <- lists:seq(1,100)],
  listen_loop(Listen).
  
listen_loop(Listen) ->  
  % ?S({accept_delay}),
  % timer:sleep(100),
  gen_http:accept_once(Listen),
  % ?S(accepting),
  receive
    {http_connection, Listen, Socket} ->
      ?S({listener,self(),sleep_after_accept}),
      timer:sleep(1000),
      Pid = spawn(fun() ->
        client_launch()
      end),
      gen_http:controlling_process(Socket, Pid),
      Pid ! {socket, Socket},
      % ?S({client,connected});
      erlang:monitor(process, Pid),
      put(clients, get(clients)+1),
      % ?S({spawned_client,get(clients)}),
      listen_loop(Listen);
    {'DOWN', _, _, _, _} ->
      put(clients, get(clients)-1),
      % ?S({died_client, get(clients)}),
      listen_loop(Listen);
    {http_error, Listen, Error} ->
      ?S({error, Error}),
      listen_loop(Listen);
    dump ->
      Requests = ets:lookup_element(http_cache, counter, 2),
      case get(prev_request_count) of
        Requests -> ok;
        _ -> ?S({requests, Requests}), put(prev_request_count, Requests)
      end,
      listen_loop(Listen);
    Else ->
      ?S(Else)
  % after
  %   5000 ->
  %     ?S({no_more_clients})    
  end.

connect(Port) ->
  spawn(fun() ->
    {ok, RR} = httpc:request("http://localhost:"++integer_to_list(Port)++"/index.html"),
    ?C({spawned,RR})
  end),
  {ok, _R1} = httpc:request("http://localhost:"++integer_to_list(Port)++"/index.html"),
  ?C(_R1),
  Post = "abcdefghikhjlpmnopqrstuvwxyz",
  % [httpc:request(post, {"http://localhost:"++integer_to_list(Port)++"/index.html", [], "application/octet-stream", Post}, [], []) || _N <- lists:seq(1,1000)],
  % {ok, _R2} = httpc:request(post, {"http://localhost:"++integer_to_list(Port)++"/index.html", [], "application/octet-stream", Post}, [], []),
  % ?C(_R2),
  {ok, _R3} = httpc:request("http://localhost:"++integer_to_list(Port)++"/index.html"),
  ?C(_R3),
  {ok, _R3} = httpc:request(put, {"http://localhost:"++integer_to_list(Port)++"/index.html", [], "text/plain", "Hi!!!\ndamn\n"}, [], []),
  ok.
  
  
client_launch() ->
  receive
    {socket, Socket} -> client_loop(Socket)
  end.

client_loop(Socket) ->
  gen_http:active_once(Socket),
  % receive
  %   {tcp, Socket, Bin} ->
  %     ?S({Bin, size(Bin)})
  % end,  
  receive
    {http, Socket, Method, URL, Keepalive, _Version, Headers} = Req ->
      ets:update_counter(http_cache, counter, 1),
      case ets:lookup(http_cache, URL) of
        [] ->
          ?S({mist,Method,URL}),
          gen_http:send(Socket, ["HTTP/1.1 404 NotFound\r\nConnection: ", keepalive(Keepalive), "\r\nContent-Length:0\r\n\r\n"]);
        [{URL, R}] when Keepalive == keepalive ->
          % ?S({chit,Method,URL}),
          % reply(Socket, R, Keepalive)
          gen_http:send(Socket, R);
        [{URL, R}] ->
          reply(Socket, R, Keepalive)
      end,
      if 
        Method == 'POST' orelse Method == 'PUT' ->
          ?S(Req),
          gen_http:receive_body(Socket),          
          Body = receive_body(Socket, []),
          ?S(Body),
          ok;
        true ->
          ok
      end,  
      if Keepalive == keepalive ->
        client_loop(Socket);
      true ->
        ok
      end;  
    {http_closed, Socket} ->
      ok;
    {http_error, Socket, timeout} ->
      ok
  end.

receive_body(Socket, Acc) ->
  gen_http:active_once(Socket),
  receive
    {http, Socket, eof} ->
      lists:reverse(Acc);
    {http, Socket, Bin} when is_binary(Bin) ->
      receive_body(Socket, [Bin|Acc]);
    Else ->
      ?S(Else)
  end.    
  
  % gen_http:active_once(Socket),
  % receive
  %   {tcp, Socket, Data} ->
  %     io:format("Data from client: ~p~n", [Data]),
  %     client_loop(Socket);
  %   Else ->
  %     io:format("Client msg: ~p~n", [Else]),
  %     ok
  % end.