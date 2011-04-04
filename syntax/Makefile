include ../Makefile.config

FILES   := xmllexer.ml camllexer.ml simplexmlparser.mli
RFILES  := basicTypes.ml \
	   simplexmlparser.ml \
	   xhtmlparser.ml xhtmlsyntax.ml

PREDEP  := xmllexer.ml camllexer.ml

LIB     := -package camlp4
PP      := -pp "camlp4of -loc loc"
PPR     := -pp "camlp4rf -loc loc"

OCAMLC   := $(OCAMLFIND) ocamlc $(DBG) $(LIB)
OCAMLOPT := $(OCAMLFIND) ocamlopt $(DBG) $(LIB)
OCAMLDEP := $(OCAMLFIND) ocamldep $(LIB)

byte: xmllexer.cmo ${LIBNAME}_syntax.cma simplexmlparser.cmo
opt: xmllexer.cmx ${LIBNAME}_syntax.cmxa simplexmlparser.cmx

OBJS := basicTypes.cmo camllexer.cmo xhtmlparser.cmo xhtmlsyntax.cmo

${LIBNAME}_syntax.cma: ${OBJS}
	$(OCAMLC) -a -o $@ $^
${LIBNAME}_syntax.cmxa: ${OBJS:.cmo=.cmx}
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
	-rm -f *~ \#* .\#*

# Dependencies
depend: ${PREDEP}
	$(OCAMLDEP) $(PP) ${FILES} > .depend
	$(OCAMLDEP) $(PPR) ${RFILES} >> .depend

FORCE:

-include .depend
