OBJECTS = common.cmo unify.cmo print.cmo builtin.cmo inference.cmo \
		grammar.cmo lexer.cmo

TESTS = test_unify test_inference test_builtin test_parser

all: mfprolog

repl: all $(TESTS)
	rlwrap ocaml $(OBJECTS)

clean:
	rm -f *.cmo *.cmi grammar.ml grammar.mli lexer.ml mfprolog $(TESTS)

re: clean all

check: all $(TESTS)
	for i in $(TESTS) ;\
	do \
		ocamlrun $$i || exit 1 ;\
	done

mfprolog: $(OBJECTS) mfprolog.cmo
	ocamlc -o mfprolog $(OBJECTS) mfprolog.cmo

%.cmo: %.ml
	ocamlc -c -o $@ $<

grammar.ml: grammar.mly
	ocamlyacc grammar.mly ;\
	ocamlc -c grammar.mli

lexer.ml: lexer.mll
	ocamllex lexer.mll

test_unify: $(OBJECTS) test_unify.ml
	ocamlc -o $@ $(OBJECTS) test_unify.ml

test_inference: $(OBJECTS) test_inference.ml
	ocamlc -o $@ $(OBJECTS) test_inference.ml

test_builtin: $(OBJECTS) test_builtin.ml
	ocamlc -o $@ $(OBJECTS) test_builtin.ml

test_parser: $(OBJECTS) test_parser.ml
	ocamlc -o $@ $(OBJECTS) test_parser.ml
