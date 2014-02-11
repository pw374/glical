###########################################################################
# glical: A library to glance at iCal data using OCaml
# (c) 2013/2014, Philippe Wang <philippe.wang@cl.cam.ac.uk>
# Licence: ISC
###########################################################################

glical:glical.cma glical_tool.cmo glical_bin.cmo
	ocamlc -o $@ $+

glical_cat:glical.cma glical_test.ml
	ocamlc -o $@ $+

%.cmi:%.mli
	ocamlc -c $<

%.cmo:%.ml %.cmi
	ocamlc -c $<

glical.cma:glical.cmo
	ocamlc -a $< -o $@

glical.cmo:glical.ml glical.cmi
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


.PHONY:clean

clean:
	rm -f *.cm[aiox] *.cmxa



# Permission to use, copy, modify, and/or distribute this software
# for any purpose with or without fee is hereby granted, provided
# that the above copyright notice and this permission notice appear
# in all copies.
# 
# THE SOFTWARE IS PROVIDED “AS IS” AND ISC DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL ISC BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY
# DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
# WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
# ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
# OF THIS SOFTWARE.
