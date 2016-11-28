.PHONY: all dirs compile clean examples eg

REBAR ?= rebar3

all: dirs deps compile

dirs:
	@mkdir -p priv/tmp

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

examples: eg
eg:
	@erlc -I deps -o ebin examples/*.erl
