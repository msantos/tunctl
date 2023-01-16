.PHONY: all dirs compile clean examples eg dialyzer typer

REBAR ?= rebar3

all: dirs compile

dirs:
	@mkdir -p priv/tmp

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

examples: eg
eg:
	@erlc -I deps -o ebin examples/*.erl

dialyzer:
	@$(REBAR) dialyzer

typer:
	@typer \
        -pa _build/default/lib/tunctl/ebin \
        -I include \
        --plt _build/default/*_plt \
        -r ./src
