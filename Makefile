all: src

src:
	rebar get-deps compile

clean:
	rebar clean

test: all
	rebar -v skip_deps=true eunit

# We assume Elixir and Quviq Quickcheck are installed
exunit:
	MIX_EXS=test/elixir/mix.exs mix test

check-syntax:
	gcc -o nul -S ${CHK_SOURCES}

.PHONY: clean src test all
