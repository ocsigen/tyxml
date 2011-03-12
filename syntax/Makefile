include ../Makefile.config

FILES   := xmllexer.ml camllexer.ml
RFILES  := basicTypes.ml \
	   simplexmlparser.ml simplexmlparser.mli \
	   xhtmlparser.ml xhtmlsyntax.ml

PREDEP  := xmllexer.ml camllexer.ml

LIB     := -package camlp4
PP      := -pp "camlp4of -loc loc"
PPR     := -pp "camlp4rf -loc loc"

OCAMLC   := $(OCAMLFIND) ocamlc $(DBG) $(LIB)
OCAMLOPT := $(OCAMLFIND) ocamlopt $(DBG) $(LIB)
OCAMLDEP := $(OCAMLFIND) ocamldep $(LIB)

byte: basicTypes.cmo xhtmlsyntax.cma simplexmlparser.cmo
opt: basicTypes.cmx xhtmlsyntax.cmxa simplexmlparser.cmx

xhtmlsyntax.cma: xmllexer.cmo camllexer.cmo xhtmlparser.cmo xhtmlsyntax.cmo
	$(OCAMLC) -a -o $@ $^

xhtmlsyntax.cmxa: xmllexer.cmx camllexer.cmx xhtmlparser.cmx xhtmlsyntax.cmx
	$(OCAMLOPT) -a -o $@ $^

# Common rules

ML   := $(filter %.ml,${FILES})
MLI  := $(filter %.mli,${FILES})
MLR  := $(filter %.ml,${RFILES})
MLIR := $(filter %.mli,${RFILES})

${MLI:.mli=.cmi}: %.cmi: %.mli
	$(OCAMLC) $(PP) -c $<
${ML:.ml=.cmo}: %.cmo: %.ml
	$(OCAMLC) $(PP) -c $<
${ML:.ml=.cmx}: %.cmx: %.ml
	$(OCAMLOPT) $(PP) -c $<

${MLIR:.mli=.cmi}: %.cmi: %.mli
	$(OCAMLC) $(PPR) -c $<
${MLR:.ml=.cmo}: %.cmo: %.ml
	$(OCAMLC) $(PPR) -c $<
${MLR:.ml=.cmx}: %.cmx: %.ml
	$(OCAMLOPT) $(PPR) -c $<

%.ml: %.mll
	$(OCAMLLEX) $<

# Clean up
clean:
	-rm -f ${PREDEP}
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
distclean: clean
	-rm -f .depend
	-rm -f *~ \#* .\#*

# Dependencies
depend: ${PREDEP}
	$(OCAMLDEP) $(PP) ${FILES} > .depend
	$(OCAMLDEP) $(PPR) ${RFILES} >> .depend

FORCE:

-include .depend
