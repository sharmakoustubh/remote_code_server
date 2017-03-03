OUTDIR=_build/default/lib/codeserver/ebin/

compile:
	rebar3 compile

test:
	rebar3 eunit

start:
	rebar3 compile
	erl -pa $(OUTDIR) -eval 'codeserver_app:start(ok, ok)'