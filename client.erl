#!/usr/bin/env escript
%%! -pa ebin

-include_lib("inets/include/httpd.hrl").
-compile(export_all).



main([URL]) ->
  make:all([load]),
  http_bench_client:run(URL).


