include ../Makefile.config
-include ../Makefile.local

FILES_F := xml_wrap.ml   \
	   xml_iter.ml   \
	   xml_print.ml  \
	   xhtml_f.ml    \
	   svg_f.ml      \
	   html5_f.ml    \

FILES := ${FILES_F} xml.ml xhtml.ml svg.ml html5.ml

DUCE_INTF  := xml_sigs_duce.mli xhtml_types_duce.mli xml_print_duce.mli xhtml_duce.mli

DUCE_FILES := xml_print_duce.ml xhtml_duce.ml

LIBS     :=

ifeq ($(HAVE_OCAML4), yes)
ANNOT := -annot -bin-annot
endif

# SYNTAX := -ppopt ../syntax/pa_include.cmo -package camlp4 -syntax camlp4o

OCAMLC   := $(OCAMLFIND) ocamlc $(ANNOT) ${DBG} ${SYNTAX}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${DBG} ${SYNTAX}
OCAMLDEP := $(OCAMLFIND) ocamldep ${SYNTAX}

OCAMLDUCEC   := ${OCAMLDUCEFIND} ocamlc ${DBG} -thread
OCAMLDUCEOPT := ${OCAMLDUCEFIND} ocamlopt ${DBG} -thread
OCAMLDUCEDEP := ${OCAMLDUCEFIND} ocamldep

all: byte opt

byte:: ${LIBNAME}.cma ${LIBNAME}_f.cma
opt:: ${LIBNAME}.cmxa ${LIBNAME}_f.cmxa
ifeq "${NATDYNLINK}" "yes"
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

ifeq "${OCAMLDUCE}" "yes"
byte:: ${LIBNAME}_duce.cma
opt:: ${LIBNAME}_duce.cmxa
ifeq "${NATDYNLINK}" "yes"
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
ifeq "${OCAMLDUCE}" "yes"
	${OCAMLDUCEDEP} ${DUCE_FILES} ${DUCE_INTF} >> .depend
endif

FORCE:

-include .depend
