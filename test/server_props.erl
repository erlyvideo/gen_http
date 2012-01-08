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

hostname() ->
	?SUCHTHAT(Hostname,
		?LET(Labels, list(hostname_label()), string:join(Labels, ".")),
		length(Hostname) > 0 andalso length(Hostname) =< 255).

port_number() ->
	choose(1, 16#ffff).

port_str() ->
	oneof(["", ?LET(Port, port_number(), ":" ++ integer_to_list(Port))]).

hostvalue() ->
  ?LET({Hostname, PortStr}, {hostname(), port_str()},
		list_to_binary(Hostname ++ PortStr)).


method() ->
  oneof(gen_http:http_method()).

path() ->
  ?SUCHTHAT(Path,
    ?LET(Labels, list(hostname_label()), "/"++string:join(Labels, "/")),
  length(Path) > 0 andalso length(Path) < 1024).

headers() ->
  [].

body() ->
  {[], <<>>}.

stub_request() ->
	?LET({Method, Path, Headers, {BodyHeaders, Body}}, {method(), path(), headers(), body()},
	#stub_request{path = Path, headers = lists:ukeysort(1, Headers ++ BodyHeaders), body = Body}).
	

make_http_request(Port, #stub_request{}) ->
  inets:start(),
  httpc:request().

receive_http_request(Socket) ->
  ok.
	

prop_httpc_requests() ->
  ?FORALL(Request, stub_request(),
	begin
	  {ok, Socket} = gen_http:listen(?PORT),
	  ok = make_http_request(?PORT, Request),
	  Request1 = receive_http_request(Socket),
	  Request == Request1
	end).
	

proper_test_() ->
    {timeout, 600,
     ?_assertEqual(true, proper:module(?MODULE, [{to_file, user},
                                                 {numtests, 1000}]))}.
