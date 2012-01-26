
DIALYZER = dialyzer
REBAR = rebar


all: compile


compile:
	./rebar compile


clean:
	./rebar clean
	@rm -f priv/*.so c_src/*.o test/*.beam

test: compile
	@$(REBAR) eunit skip_deps=true

ct:
	@$(REBAR) ct

build-plt:
	@$(DIALYZER) --build_plt --output_plt .gen_http.plt \
		--apps kernel stdlib sasl inets crypto public_key ssl

dialyze:
	@$(DIALYZER) ebin/gen_http.beam --plt .gen_http.plt \
		-Werror_handling \
		-Wrace_conditions -Wunmatched_returns -Wunderspecs #-Wbehaviours

.PHONY: test