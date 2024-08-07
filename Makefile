REBAR ?= rebar

IS_REBAR3:=$(shell expr `$(REBAR) --version | awk -F '[ .]' '/rebar / {print $$2}'` '>=' 3)

ERL=erl

all: src

src:
	$(REBAR) get-deps
	$(REBAR) compile

clean:
	$(REBAR) clean

ifeq "$(IS_REBAR3)" "1"
test:
	$(REBAR) eunit -v

spec:
	$(ERL) -noinput +B -pa _build/default/lib/fast_xml/ebin -eval 'case fxml_gen:compile("spec/fxmlrpc_codec.spec", [{erl_dir, "src"}, {hrl_dir, "include"}]) of ok -> halt(0); _ -> halt(1) end.'
else
test: all
	$(REBAR) -v skip_deps=true eunit
endif

spec:
	$(ERL) -noinput +B -pa ebin -pa deps/*/ebin -eval 'case fxml_gen:compile("spec/fxmlrpc_codec.spec", [{erl_dir, "src"}, {hrl_dir, "include"}]) of ok -> halt(0); _ -> halt(1) end.'

check-syntax:
	gcc -o nul -S ${CHK_SOURCES}

.PHONY: clean src test all spec
