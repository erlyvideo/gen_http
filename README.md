gen_http driver for erlang
==========================

This module is a high efficient special linked-in driver that handled HTTP on C level.

It uses http://github.com/joyent/http-parser/ code based on Nginx http parser, which is beleived
to be one of the fastest on earth.

`gen_http` has non-gen_tcp API. It has other call for opening socket, accepting connection and receiving data.

`gen_http` is built around idea of handling HTTP protocol.


```erlang
% listener.erl:
{ok, Listener} = gen_http:listen(8080, [{reuseaddr,true},{backlog,400}]),
microtcp:accept_once(Socket), % For accepting next connection
receive
  {http_connection, Listen, Socket} ->
    Pid = spawn(fun() -> handle_client() end),
    gen_http:controlling_process(Socket, Pid),
    Pid ! {socket, Socket},
    receive
      {done, Pid}
    end
end.

handle_client() ->
  Socket = receive
    {socket, S} -> S
  end,
  client_loop(Socket).

client_loop(Socket) ->
  gen_http:active_once(Socket),
  receive
  {http, Socket, Method, URL, Keepalive, Version, Headers} = Req ->
    gen_http:send(Socket, "HTTP/1.1 200 OK\r\n\r\n"),
    client_loop(Socket);
    {http_closed, Socket} ->
      ok;
    {http_error, Socket, Error} ->
      gen_http:close(Socket)  
  end.
```


Usage
=====

gen_http can be used in server and in client mode.


Server mode
-----------

Server mode is divided in two parts: listener and worker.

Listener socket is created with `{ok, Listen} = gen_http:listen(Port, Options)` call.

Then listener socket must be activated with `gen_http:accept_once(Listen)` call.
This call may be done from many processes, multiaccept is supported.

Listener socket will send message `{http_connection, Listen, Client}` to some of accepting processes.
Synchronous call `accept(Listen, Timeout)` is emulated via `receive .. after ..`

Standard call `gen_http:controlling_process(Client, HandlerPid)` is supported.

After receiving socket it is handler task to work with it.

Handler mode when being server is very similar to client mode. Slight difference in request/response message format.


Client mode
-----------

To establish connection to remote server `{ok, Socket} = gen_http:connect(Host, Port, Options)` should be used.

After this call make request: `gen_http:send(Socket, ["GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"])`

When this call returns, use next chapter API, because socket enters in the same `{active,once}` state, as in handler mode.


Client/Handler mode
-------------------

After receiving request or response headers handler process should call `gen_http:active_once(Socket)` to receive messages from socket.

Socket can send following list of messages:

* `{http, Socket, Method, URL, Keepalive, Version, Headers}` received when remote peer sends *request* to us (in server mode);
* `{http, Socket, Status, Keepalive, Version, Headers}` received when remote peer replies with some *response* to us (in client mode);
* `{http, Socket, Bin} when is_binary(Bin)` part of body;
* `{http, Socket, eof}` when body is over;
* `{http, Socket, connected}` when remote server accepts our connection and we can send HTTP *request* there;
* `{http, Socket, empty}` when socket has sent all scheduled data to remote peer. It can be used to emulate blocking `gen_http:send(Socket, Data)` call;
* `{http_error, Socket, Error}` when some error has happened;
* `{http_closed, Socket}` after socket was closed. It can be received for example when accepting

Mention Keepalive option. It can be `keepalive` or `close`. Use this value to determine whether you should close connection or send next message
on this wire.


Receiving body issues
---------------------

If you wish to receive body, than take a look at `gen_http:setopts(Socket, [{chunk_size,ChunkSize}])`.

`gen_tcp` driver has performance problems in `active` mode: messages are received of MTU size. If you download 1GB file, you may
receive hundreds of thousands small messages, while you require only several tens of them, but big.

`gen_http` driver will try to accumulate binaries in rather efficient way inside in chunks not more than ChunkSize + 10KB (default buffer size).
Don't rely that ChunkSize is maximum size. You can receive more data than you have asked to, but not too much.

Also received data may be less than ChunkSize if body is over. Mention, that there is no timeout for sending if remote peer doesn't send data.
It may highly increase latency. Sorry, but you are worried.






























