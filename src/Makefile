OBJECTS = common.cmo unify.cmo print.cmo inference.cmo

TESTS = test_unify test_inference

all: mfprolog

clean:
	rm -f *.cmo *.cmi mfprolog

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

test_unify: $(OBJECTS) test_unify.ml
	ocamlc -o $@ $(OBJECTS) test_unify.ml

test_inference: $(OBJECTS) test_inference.ml
	ocamlc -o $@ $(OBJECTS) test_inference.ml
