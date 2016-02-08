CACHEGRIND=qcachegrind
ELVIS=./bin/elvis
REBAR=./bin/rebar3

all: compile

clean:
	@echo "Running rebar3 clean..."
	@$(REBAR) clean -a

compile:
	@echo "Running rebar3 compile..."
	@$(REBAR) as compile compile

coveralls:
	@echo "Running rebar3 coveralls send..."
	@$(REBAR) as test coveralls send

dialyzer:
	@echo "Running rebar3 dialyze..."
	@$(REBAR) dialyzer

edoc:
	@echo "Running rebar3 edoc..."
	@$(REBAR) as edoc edoc

elvis:
	@echo "Running elvis rock..."
	@$(ELVIS) rock

eunit:
	@echo "Running rebar3 eunit..."
	@$(REBAR) do eunit -cv, cover -v

profile:
	@echo "Profiling..."
	@$(REBAR) as test compile
	@erl -noshell \
	     -pa _build/test/lib/*/ebin \
		 -eval 'marina_profile:fprofx()' \
		 -eval 'init:stop()'
	@_build/test/lib/fprofx/erlgrindx -p fprofx.analysis
	@$(CACHEGRIND) fprofx.cgrind

test:  elvis xref eunit dialyzer

travis:  elvis xref eunit dialyzer coveralls

xref:
	@echo "Running rebar3 xref..."
	@$(REBAR) xref

.PHONY: clean compile coveralls dialyzer edoc elvis eunit profile test xref
