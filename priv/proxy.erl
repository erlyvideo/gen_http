#!/usr/bin/env escript
%%! -pa ebin  -smp enable +K true +A 16 +a 2048



main([Upstream]) ->
  micro_proxy:listen(Upstream).
 
