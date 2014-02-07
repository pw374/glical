

glical:glical.cma glical_test.ml
	ocamlc -o $< $+

glical.cma:glical.cmo
	ocamlc -a $< -o $@

glical.cmo:glical.ml
	${MAKE} glical.cmi
	ocamlc -c $<

glical.cmi:glical.mli
	ocamlc -c $<



glical.opt:glical.cma glical_test.ml
	ocamlopt -o $< $+

glical.cmxa:glical.cmx
	ocamlopt -a $< -o $@

glical.cmx:glical.ml
	${MAKE} glical.cmi
	ocamlopt -c $<


