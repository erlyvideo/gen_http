%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        HTTP linked-in driver
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to
%%% deal in the Software without restriction, including without limitation the
%%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%%% sell copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%% 
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%%% IN THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(gen_http).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

% Server API
-export([listen/2, listen/1, controlling_process/2, active_once/1, send/2, close/1, peername/1]).
-export([receive_body/2]).
-export([accept_once/1]).

% Client API
-export([connect/3, connect/2]).

listen(Port) -> listen(Port, []).

-define(CMD_LISTEN, 1).
-define(CMD_ACTIVE_ONCE, 2).
-define(CMD_RECEIVE_BODY, 3).
-define(CMD_STATS, 4).
-define(CMD_ACCEPT_ONCE, 5).

listen(Port, Options) ->
  Path = case code:lib_dir(gen_http,priv) of
    P when is_list(P) -> P;
    _ -> filename:dirname(code:which(?MODULE))++"/../priv/"
  end,
  case erl_ddll:load_driver(Path, gen_http_drv) of
  	ok -> ok;
  	{error, already_loaded} -> ok;
  	{error, Error} -> exit({error, {could_not_load_driver,erl_ddll:format_error(Error)}})
  end,
  Socket = open_port({spawn, gen_http_drv}, [binary]),
  Reuseaddr = case proplists:get_value(reuseaddr, Options, true) of
    true -> 1;
    _ -> 0
  end,
  Keepalive = case proplists:get_value(keepalive, Options) of
    true -> 1;
    _ -> 0
  end,
  Timeout = proplists:get_value(timeout, Options, 60000),
  Backlog = proplists:get_value(backlog, Options, 30),
  
  <<"ok">> = port_control(Socket, ?CMD_LISTEN, <<Port:16, Backlog:16/little, Reuseaddr, Keepalive, Timeout:16/little>>),
  {ok, Socket}.


controlling_process(Socket, NewOwner) when is_port(Socket), is_pid(NewOwner) ->
  case erlang:port_info(Socket, connected) of
	  {connected, Pid} when Pid =/= self() ->
	    {error, not_owner};
	  undefined ->
	    {error, einval};
	_ ->
		try erlang:port_connect(Socket, NewOwner) of
		  true -> 
			  unlink(Socket), %% unlink from port
				ok
		catch
			error:Reason -> 
				{error, Reason}
		end
  end.

receive_body(Socket, ChunkSize) ->
  <<"ok">> = port_control(Socket, ?CMD_RECEIVE_BODY, <<ChunkSize:32/little>>),
  ok.
  

peername(Socket) when is_port(Socket) ->
  {ok, {{0,0,0,0}, 4000}}.

close(Socket) when is_port(Socket) ->
  erlang:port_close(Socket).

active_once(Socket) ->
  port_control(Socket, ?CMD_ACTIVE_ONCE, <<>>).

accept_once(Socket) ->
  port_control(Socket, ?CMD_ACCEPT_ONCE, <<>>).


send(Socket, Bin) when is_port(Socket) ->
  port_command(Socket, Bin).


connect(Host, Port) ->
  connect(Host, Port, []).
  
connect(_Host, _Port, _Options) ->
  erlang:throw(not_implemented).





