-module(micro_proxy).

-compile(export_all).
-define(D(X), io:format("~p ~p~n", [?LINE,X])).

-compile({no_auto_import,[now/0]}).

listen(Upstream) ->
  {ok, Listen} = microtcp:listen(9000, [{backlog,4000}]),
  ets:new(http_cache, [set,named_table,public]),
  inets:start(),
  spawn(fun() -> reloader(Upstream) end),
  timer:send_interval(1000, dump),
  listen_loop(Listen, Upstream).


listen_loop(Listen, Upstream) ->
  microtcp:active_once(Listen),
  receive
    {http_connection, Listen, Socket} ->
      Pid = spawn(fun() ->
        client_launch(Upstream)
      end),
      microtcp:controlling_process(Socket, Pid),
      Pid ! {socket, Socket},
      listen_loop(Listen, Upstream);
    dump ->
      <<Count:32>> = port_control(Listen, 4, <<>>),
      ?D({requests,Count}),
      listen_loop(Listen, Upstream);
    Else ->
      ?D(Else)
  end.

now() ->
  timer:now_diff(os:timestamp(),{0,0,0}) div 1000000.


reloader(Upstream) ->
  timer:sleep(1000),
  Now = now(),
  [load(Upstream, URL) || {URL, _, Expires} <- ets:tab2list(http_cache), Expires < Now],
  reloader(Upstream).



load_and_save(Upstream, URL) ->
  ?D({refetch,URL}),
  {ok, {{Proto,Code,Msg}, Headers, Reply}} = httpc:request("http://"++Upstream++binary_to_list(URL)),
  Bin = iolist_to_binary([
    io_lib:format("~s ~p ~s\r\n", [Proto,Code,Msg]),
    [[K, ": ", V, "\r\n"] || {K,V} <- Headers],
    "\r\n",
    Reply
  ]),
  {match, [Expiry]} = re:run(proplists:get_value("cache-control", Headers, "max-age=36000"), "max-age=(\\d+)", [{capture,all_but_first,list}]),
  Expires = now() + list_to_integer(Expiry),
  ets:insert(http_cache, {URL, Bin, Expires}),
  Bin.

load(Upstream, URL) ->
  load_and_save(Upstream, URL).

client_launch(Upstream) ->
  receive
    {socket, Socket} -> client_loop(Socket, Upstream)
  end.

client_loop(Socket, Upstream) ->
  microtcp:active_once(Socket),
  receive
    {http, Socket, _Method, URL, _Keepalive, _ReqHeaders} ->
      % ?D({get,URL}),
      case ets:lookup(http_cache, URL) of
        [{URL, Reply, _Expires}] ->
          microtcp:send(Socket, Reply);
        [] ->
          Reply = load(Upstream, URL),
          microtcp:send(Socket, Reply)
      end,
      client_loop(Socket, Upstream);
    {http, Socket, _} ->
      client_loop(Socket, Upstream);
    {http_closed, Socket} ->
      ok;  
    Else ->
      ?D(Else)
  end.
  