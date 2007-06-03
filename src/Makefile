all: mfprolog

clean:
	rm -f *.cmo *.cmi mfprolog

mfprolog: common.cmo unify.cmo mfprolog.cmo
	ocamlc -o mfprolog $<

%.cmo: %.ml
	ocamlc -c -o $@ $<
