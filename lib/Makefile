include ../Makefile.config
-include ../Makefile.local

FILES_F := uri.ml        \
	   XML_iter.ml   \
	   XML_print.ml  \
	   XHTML_f.ml    \
	   SVG_f.ml      \
	   HTML5_f.ml    \

FILES := ${FILES_F} XML.ml XHTML.ml SVG.ml HTML5.ml

DUCE_INTF  := XML_sigs_duce.mli XHTML_types_duce.mli XML_print_duce.mli XHTML_duce.mli

DUCE_FILES := XML_print_duce.ml XHTML_duce.ml

LIBS     := -package netstring

# SYNTAX := -ppopt ../syntax/pa_include.cmo -package camlp4 -syntax camlp4o

OCAMLC   := $(OCAMLFIND) ocamlc ${SYNTAX}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${SYNTAX}
OCAMLDEP := $(OCAMLFIND) ocamldep ${SYNTAX}

OCAMLDUCEC   := ${OCAMLDUCEFIND} ocamlc${BYTEDBG} -thread
OCAMLDUCEOPT := ${OCAMLDUCEFIND} ocamlopt ${OPTDBG} -thread
OCAMLDUCEDEP := ${OCAMLDUCEFIND} ocamldep

all: byte opt

byte:: ${LIBNAME}.cma ${LIBNAME}_f.cma
opt:: ${LIBNAME}.cmxa ${LIBNAME}_f.cmxa
ifeq "${NATDYNLINK}" "YES"
opt:: ${LIBNAME}.cmxs ${LIBNAME}_f.cmxs
endif

${LIBNAME}.cma: $(FILES:.ml=.cmo)
	$(OCAMLC) -a $^ -o $@

${LIBNAME}.cmxa: $(FILES:.ml=.cmx)
	$(OCAMLOPT) -a $^ -o $@

${LIBNAME}_f.cma: $(FILES_F:.ml=.cmo)
	$(OCAMLC) -a $^ -o $@

${LIBNAME}_f.cmxa: $(FILES_F:.ml=.cmx)
	$(OCAMLOPT) -a $^ -o $@

##########

ifeq "${OCAMLDUCE}" "YES"
byte:: ${LIBNAME}_duce.cma
opt:: ${LIBNAME}_duce.cmxa
ifeq "${NATDYNLINK}" "YES"
opt:: ${LIBNAME}_duce.cmxs
endif
endif

${LIBNAME}_duce.cma: ${DUCE_FILES:.ml=.cmo}
	${OCAMLC} -a -o $@ $^
${LIBNAME}_duce.cmxa: ${DUCE_FILES:.ml=.cmx}
	${OCAMLOPT} -a -o $@ $^

##########

${DUCE_INTF:.mli=.cmi}: %.cmi: %.mli
	$(OCAMLDUCEC) ${LIBS} -c $<
${DUCE_FILES:.ml=.cmo}: %.cmo: %.ml
	$(OCAMLDUCEC) ${LIBS} -c $<
${DUCE_FILES:.ml=.cmx}: %.cmx: %.ml
	$(OCAMLDUCEOPT) ${LIBS} -c $<

%.cmi: %.mli
	$(OCAMLC) ${LIBS} -c $<
%.cmo: %.ml
	$(OCAMLC) ${LIBS} -c $<
%.cmx: %.ml
	$(OCAMLOPT) ${LIBS} -c $<
%.cmxs: %.cmxa
	$(OCAMLOPT) -shared -linkall -o $@ $<

# Clean up
clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
distclean: clean
	-rm -f *~ \#* .\#*

depend:
	$(OCAMLDEP) $(filter-out ${DUCE_INTF}, ${wildcard *.mli}) > .depend
	$(OCAMLDEP) $(filter-out ${DUCE_FILES}, ${wildcard *.ml}) >> .depend
ifeq "${OCAMLDUCE}" "YES"
	${OCAMLDUCEDEP} ${DUCE_FILES} ${DUCE_INTF} >> .depend
endif

FORCE:

-include .depend
