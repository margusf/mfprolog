all: mfprolog

clean:
	rm -f *.cmo *.cmi mfprolog

re: clean all

mfprolog: common.cmo unify.cmo print.cmo inference.cmo mfprolog.cmo
	ocamlc -o mfprolog $<

%.cmo: %.ml
	ocamlc -c -o $@ $<
