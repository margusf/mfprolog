OBJECTS = common.cmo unify.cmo print.cmo builtin.cmo inference.cmo \
		grammar.cmo lexer.cmo

UNIT_TESTS = test_helper.ml test_parser.ml test_unify.ml \
		test_inference.ml test_builtin.ml

OCAMLUNIT = $(shell ocamlfind query oUnit)

all: mfprolog

repl: all repl_helper.cmo
	rlwrap ocaml $(OBJECTS) repl_helper.cmo

clean:
	rm -f *.cmo *.cmi grammar.ml grammar.mli lexer.ml mfprolog test_suite

re: clean all

check: test_suite
	./test_suite -verbose

test_suite: $(OBJECTS) $(UNIT_TESTS) test_suite.ml
	ocamlfind ocamlc -o test_suite -package oUnit -linkpkg \
		$(OBJECTS) $(UNIT_TESTS) test_suite.ml

mfprolog: $(OBJECTS) mfprolog.cmo
	ocamlc -o mfprolog $(OBJECTS) mfprolog.cmo

%.cmo: %.ml
	ocamlc -c -o $@ $<

grammar.ml: grammar.mly
	ocamlyacc grammar.mly ;\
	ocamlc -c grammar.mli

lexer.ml: lexer.mll
	ocamllex lexer.mll
