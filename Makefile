all: src

src:
	rebar compile

clean:
	rebar clean

test: all
	rebar -v skip_deps=true eunit

check-syntax:
	gcc -o nul -S ${CHK_SOURCES}

.PHONY: clean src test all
