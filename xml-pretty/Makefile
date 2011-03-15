include ../Makefile.config

FILES = xhtml_format.ml xhtml_mkcompact.ml xhtml_mkpretty.ml \
	xhtmlpretty.ml xhtmlcompact.ml \
	xhtml5pretty.ml xhtml5compact.ml

FILES_STREAM = xhtml_streams.ml \
        xhtml_mkpretty_streams.ml xhtml_mkcompact_streams.ml \
	xhtmlpretty_streams.ml xhtmlcompact_streams.ml \
	xhtml5pretty_streams.ml xhtml5compact_streams.ml

LIB      := -package camlp4 -I ../xhtml

OCAMLC   := $(OCAMLFIND) ocamlc $(DBG) $(LIB)
OCAMLOPT := $(OCAMLFIND) ocamlopt $(DBG) $(LIB)
OCAMLDEP := $(OCAMLFIND) ocamldep $(LIB)

byte: xhtmlpretty.cma $(FILES_STREAM:.ml=.cmo)
opt: xhtmlpretty.cmxa $(FILES_STREAM:.ml=.cmx)

xhtmlpretty.cma: $(FILES:.ml=.cmo)
	$(OCAMLC) -a -o $@ $^
xhtmlpretty.cmxa: $(FILES:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ $^

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx
.ml.cmo:
	$(OCAMLC) -c $<
.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmx:
	$(OCAMLOPT) -c $<

# Clean up
clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
distclean: clean
	-rm -f .depend
	-rm -f *~ \#* .\#*

depend:
	$(OCAMLDEP) *.ml *.mli > .depend

-include .depend
