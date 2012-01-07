
all: compile


compile:
	./rebar compile


clean:
	./rebar clean

test:
	erl -make
	erl -pa ebin -noinput -s gen_http test -s init stop
