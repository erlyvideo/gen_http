
DIALYZER = dialyzer
REBAR = rebar


all: compile


compile:
	./rebar compile


clean:
	./rebar clean

test:
	erl -make
	erl -pa ebin -noinput -s gen_http test -s init stop

build-plt:
	@$(DIALYZER) --build_plt --output_plt .cowboy_dialyzer.plt \
		--apps kernel stdlib sasl inets crypto public_key ssl

dialyze:
	@$(DIALYZER) --src src --plt .cowboy_dialyzer.plt \
		-Wbehaviours -Werror_handling \
		-Wrace_conditions -Wunmatched_returns # -Wunderspecs
