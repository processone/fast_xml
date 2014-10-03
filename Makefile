all: src

src:
	rebar compile

clean:
	rebar clean

test:
	rebar eunit

.PHONY: clean src test
