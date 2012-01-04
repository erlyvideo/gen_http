-module(micro_proxy).

-compile(export_all).

listen(Upstream) ->
  {ok, Listen} = microtcp:listen(9000, [{backlog,4000}]),
  ets:new(http_cache, [set,named_table,public]),
  inets:start(),
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
    Else ->
      ?D(Else)
  end.


client_launch(Upstream) ->
  receive
    {socket, Socket} -> client_loop(Socket, Upstream)
  end.

client_loop(Socket, Upstream) ->
  microtcp:active_once(Socket),
  receive
    {http, Socket, Method, URL, Keepalive, _ReqHeaders} = Req ->
      Now = timer:now_diff(os:timestamp(),{0,0,0}) div 1000000,
      case ets:lookup(http_cache, URL) of
        [{URL, Reply, Expires}] when Expires > Now ->
          microtcp:send(Socket, Reply);
        _ ->
          ?D(URL),
          {ok, {{Proto,Code,Msg}, Headers, Reply}} = httpc:request("http://"++Upstream++binary_to_list(URL)),
          Bin = iolist_to_binary([
            io_lib:format("~s ~p ~s\r\n", [Proto,Code,Msg]),
            [[K, ": ", V, "\r\n"] || {K,V} <- Headers],
            "\r\n",
            Reply
          ]),
          {match, [Expiry]} = re:run(proplists:get_value("cache-control", Headers, "max-age=36000"), "max-age=(\\d+)", [{capture,all_but_first,list}]),
          Expires = Now + list_to_integer(Expiry),
          ets:insert(http_cache, {URL, Bin, Expires}),
          microtcp:send(Socket, Bin)
      end,
      client_loop(Socket, Upstream);
    {http, Socket, _} ->
      client_loop(Socket, Upstream);
    {http_closed, Socket} ->
      ok;  
    Else ->
      ?D(Else)
  end.
  