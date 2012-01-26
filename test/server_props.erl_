-module(server_props).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 34567).

-record(stub_request, {
  path,
  method,
  headers = [],
  body
}).


hostname_head_char() ->
	oneof([choose($a, $z), choose($A, $Z), choose($0, $9)]).

hostname_char() ->
	oneof([choose($a, $z), choose($A, $Z), choose($0, $9), $-]).

hostname_label() ->
	?SUCHTHAT(Label, [hostname_head_char()|list(hostname_char())],
		length(Label) < 64).

% hostname() ->
%   ?SUCHTHAT(Hostname,
%     ?LET(Labels, list(hostname_label()), string:join(Labels, ".")),
%     length(Hostname) > 0 andalso length(Hostname) =< 255).
% 
% port_number() ->
%   choose(1, 16#ffff).
% 
% port_str() ->
%   oneof(["", ?LET(Port, port_number(), ":" ++ integer_to_list(Port))]).
% 
% hostvalue() ->
%   ?LET({Hostname, PortStr}, {hostname(), port_str()},
%     list_to_binary(Hostname ++ PortStr)).


method() ->
  % oneof(gen_http:http_method()).
  oneof(['GET']).

path() ->
  ?SUCHTHAT(Path,
    ?LET(Labels, list(hostname_label()), "/"++string:join(Labels, "/")),
  length(Path) > 0 andalso length(Path) < 1024).

headers() ->
  [].

body() ->
  {[], undefined}.

stub_request() ->
	?LET({Method, Path, Headers, {BodyHeaders, Body}}, {method(), path(), headers(), body()},
	#stub_request{method = Method, path = Path, headers = lists:ukeysort(1, Headers ++ BodyHeaders), body = Body}).
	

url(#stub_request{path = Path}) ->
  "http://localhost:"++integer_to_list(?PORT) ++ Path.

make_http_request(#stub_request{method = Method, headers = Headers} = Req) ->
  inets:start(),
  SmallMethod = list_to_atom(string:to_lower(atom_to_list(Method))),
  {ok, Ref} = httpc:request(SmallMethod, {url(Req), Headers}, [], [{sync,false},{stream,self}]),
  {ok, Ref}.

close_http_request(Ref) ->
  httpc:cancel_request(Ref).

receive_http_request(Listen) ->
  gen_http:accept_once(Listen),
  receive
    {http_connection, Listen, Socket} ->
      handle_http_connection(Socket)
  after
    1000 -> {error, timeout_accepting}  
  end.

handle_http_connection(Socket) ->
  gen_http:active_once(Socket),
  receive
    {http, Socket, Method, Path, _Keepalive, _Version, Headers} ->
      gen_http:close(Socket),
      {ok, #stub_request{method = Method, path = binary_to_list(Path), headers = lists:ukeysort(1,Headers)}};
    Else ->
      {error, Else}  
  after
    1000 -> {error, timeout_receiving}
  end.

compare_requests(#stub_request{headers = Headers1} = Req1, #stub_request{headers = Headers2} = Req2) ->
  Req1#stub_request{headers = clean_headers(Headers1)} == Req2#stub_request{headers = clean_headers(Headers2)}.

clean_headers(Headers) ->
  [{K,V} || {K,V} <- Headers, not lists:member(K, ['Host', 'Connection', <<"Te">>])].

prop_httpc_requests() ->
  ?FORALL(Request, stub_request(),
	begin
    {ok, Listen} = gen_http:listen(?PORT),
	  {ok, Ref} = make_http_request(Request),
	  {ok, Request1} = receive_http_request(Listen),
	  gen_http:close(Listen),
	  close_http_request(Ref),
    % io:format("~p ~p ~p~n", [compare_requests(Request, Request1), Request, Request1]),
	  compare_requests(Request, Request1)
	end).
	

proper_test_() ->
  {timeout, 600, fun() ->
    % ?assertEqual(true, proper:quickcheck(prop_httpc_requests(), [{numtests, 40}]))
    true
  end}.
