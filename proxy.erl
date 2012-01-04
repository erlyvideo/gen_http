#!/usr/bin/env escript
%%! -pa ebin  -smp enable +K true +A 16 +a 2048

-define(D(X), io:format("~p ~p~n", [?LINE,X])).


main([Upstream]) ->
  micro_proxy:listen(Upstream).
 
