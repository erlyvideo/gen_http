-module(server_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
  [{group, listener}, {group, handler}].


groups() ->
  [{listener, [], [accept]}, 
  {handler, [], []}].


init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

init_per_group(_Group, Config) ->
  Port = 30842,
  {ok, L} = gen_http:listen(Port),
  [{port,Port},{socket,L}|Config].

end_per_group(_Group, Config) ->
  {socket, Socket} = lists:keyfind(socket, 1, Config),
  gen_http:cache_clear(Socket),
  gen_http:close(Socket),
  ok.

accept(Config) ->
  {socket, Socket} = lists:keyfind(socket, 1, Config),
  {port, Port} = lists:keyfind(port, 1, Config),
  {ok, Cli} = gen_tcp:connect("localhost", Port, [binary, {active,false}, {packet,raw}]),
  receive
    {http_connection, Socket, Client} ->
      gen_http:send(Client, "true"),
      {ok, <<"true">>} = gen_tcp:recv(Cli, 4),
      gen_http:close(Client)
  after
    1000 -> erlang:exit(timeout)
  end,
  gen_tcp:close(Cli),
  gen_http:close(Socket),
  ok.
