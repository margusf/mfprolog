OBJECTS = common.cmo utils.cmo unify.cmo print.cmo grammar.cmo lexer.cmo loading.cmo \
		arithmetic.cmo compare.cmo builtin.cmo inference.cmo repl.cmo 

UNIT_TESTS = test_helper.ml test_parser.ml test_unify.ml \
		test_inference.ml test_builtin.ml test_arithmetic.ml

OCAMLUNIT = $(shell ocamlfind query oUnit)

OCAMLC = ocamlc -dtypes -w p

all: mfprolog

repl: all repl_helper.cmo
	rlwrap ocaml $(OBJECTS) repl_helper.cmo

clean:
	rm -f *.cmo *.cmi grammar.ml grammar.mli lexer.ml mfprolog test_suite

re: clean all

check: test_suite
	./test_suite -verbose

test_suite: $(OBJECTS) $(UNIT_TESTS) test_suite.ml
	ocamlfind $(OCAMLC) -o test_suite -package oUnit -linkpkg \
		$(OBJECTS) $(UNIT_TESTS) test_suite.ml

mfprolog: $(OBJECTS) mfprolog.cmo
	$(OCAMLC) -o mfprolog $(OBJECTS) mfprolog.cmo

%.cmo: %.ml
	$(OCAMLC) -c -o $@ $<

grammar.ml: grammar.mly
	ocamlyacc grammar.mly ;\
	$(OCAMLC) -c grammar.mli

lexer.ml: lexer.mll
	ocamllex lexer.mll
