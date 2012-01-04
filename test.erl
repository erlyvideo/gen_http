#!/usr/bin/env ERL_LIBS=.. escript
%%! -pa ebin

% a %%! -pa ebin  -smp enable +K true +A 16 +a 2048

-define(C(X), io:format("client:~p ~p~n", [?LINE,X])).
-define(S(X), io:format("server:~p ~p~n", [?LINE,X])).

main(Args) ->
  inets:start(),
  Listener = spawn(fun() ->
    listen(9000)
  end),
  erlang:monitor(process, Listener),
  
  case lists:member("bench", Args) of
    true -> ok;
    false ->
  Client = spawn(fun() ->
    connect(9000)
  end),
  erlang:monitor(process, Client),
  
  receive
    {'DOWN', _, process, Client, Reason1} -> io:format("client died ~p~n", [Reason1])
  end,
  Listener ! stop
  end,
  receive
    {'DOWN', _, process, Listener, Reason2} -> io:format("listener died ~p~n", [Reason2])
  end,
  ok.

-define(SIZE, 100000).

keepalive(keepalive) -> "Keep-Alive";
keepalive(close) -> "Close".

reply(Socket, Reply, Keepalive) ->
  microtcp:send(Socket, reply(Reply, Keepalive)).

reply(Reply, Keepalive) ->
  iolist_to_binary([
    "HTTP/1.1 200 OK\r\n",
    "Connection: ",keepalive(Keepalive), "\r\n",
    io_lib:format("Content-Length: ~p\r\n", [iolist_size(Reply)]),
    "\r\n",
    Reply
  ]).

listen(Port) ->
  Opts1 = [binary, {packet, raw}, {reuseaddr, true}, 
          {keepalive, true}, {backlog, 800}, {active, once}],
  {ok, Listen} = microtcp:listen(Port, Opts1),
  ?S({open_port,Listen}),
  % Bin = crypto:rand_bytes(?SIZE),
  Bin = ["Hello World!\n" || _ <- lists:seq(1,1000)],
  timer:send_interval(1000, dump),
  
  ets:new(http_cache, [set,named_table,public]),
  ets:insert(http_cache, {<<"/index.html">>, reply(Bin, keepalive)}),
  ets:insert(http_cache, {counter, 0}),
  put(prev_request_count, 0),
  put(clients, 0),
  % Spawners = [spawn_link(fun() ->
  %   listen(Listen)
  % end) || _N <- lists:seq(1,100)],
  listen_loop(Listen).
  
listen_loop(Listen) ->  
  % ?S({accept_delay}),
  % timer:sleep(100),
  microtcp:active_once(Listen),
  % ?S(accepting),
  receive
    {http_connection, Listen, Socket} ->
      Pid = spawn(fun() ->
        client_launch()
      end),
      microtcp:controlling_process(Socket, Pid),
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
    stop ->
      microtcp:close(Listen);  
    Else ->
      ?S(Else)
  % after
  %   5000 ->
  %     ?S({no_more_clients})    
  end.

connect(Port) ->
  {ok, _R1} = httpc:request("http://localhost:"++integer_to_list(Port)++"/index.html"),
  ?C(_R1),
  Post = "abcdefghikhjlpmnopqrstuvwxyz",
  [httpc:request(post, {"http://localhost:"++integer_to_list(Port)++"/index.html", [], "application/octet-stream", Post}, [], []) || _N <- lists:seq(1,1000)],
  {ok, _R2} = httpc:request(post, {"http://localhost:"++integer_to_list(Port)++"/index.html", [], "application/octet-stream", Post}, [], []),
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
  microtcp:active_once(Socket),
  % receive
  %   {tcp, Socket, Bin} ->
  %     ?S({Bin, size(Bin)})
  % end,  
  receive
    {http, Socket, Method, URL, Keepalive, Headers} = Req ->
      ets:update_counter(http_cache, counter, 1),
      case ets:lookup(http_cache, URL) of
        [] ->
          ?S({mist,Method,URL}),
          microtcp:send(Socket, ["HTTP/1.1 404 NotFound\r\nConnection: ", keepalive(Keepalive), "\r\nContent-Length:0\r\n\r\n"]);
        [{URL, R}] when Keepalive == keepalive ->
          % ?S({chit,Method,URL}),
          % reply(Socket, R, Keepalive)
          microtcp:send(Socket, R);
        [{URL, R}] ->
          reply(Socket, R, Keepalive)
      end,
      if 
        Method == 'POST' orelse Method == 'PUT' ->
          ?S(Req),
          microtcp:receive_body(Socket, 1024),          
          Body = receive_body(Socket, []),
          ?S(Body),
          ok;
        true ->
          ok
      end,  
      if Keepalive == keepalive ->
        client_loop(Socket);
      true ->
        microtcp:close(Socket),
        ok
      end;  
    {http_closed, Socket} ->
      ok;
    {http_error, Socket, timeout} ->
      microtcp:close(Socket)  
  end.

receive_body(Socket, Acc) ->
  microtcp:active_once(Socket),
  receive
    {http, Socket, eof} ->
      lists:reverse(Acc);
    {http, Socket, Bin} when is_binary(Bin) ->
      receive_body(Socket, [Bin|Acc]);
    Else ->
      ?S(Else)
  end.    
  
  % microtcp:active_once(Socket),
  % receive
  %   {tcp, Socket, Data} ->
  %     io:format("Data from client: ~p~n", [Data]),
  %     client_loop(Socket);
  %   Else ->
  %     io:format("Client msg: ~p~n", [Else]),
  %     ok
  % end.