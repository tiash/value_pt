REBAR=rebar
DIALYZER=dialyzer
all: compile

dependencies: 
	@${REBAR} get-deps update-deps

clean:
	@${REBAR} clean

compile: 
	@${REBAR} compile


xref: compile
	@${REBAR} xref # don't scan dependencies...

check: compile xref dialyze test

doc:
	@${REBAR} doc

test: compile
	@${REBAR} eunit

plts=erts.plt kernel.plt stdlib.plt syntax_tools.plt

$(plts): %.plt:
	@${DIALYZER} --build_plt --output_plt $@ --apps $*

plt: $(plts)

dialyze:
	@${DIALYZER} -pa ebin --plts $(plts) -I include --src -r src



