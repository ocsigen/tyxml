include ../Makefile.config
include ../Makefile.filelist

LIB := -I ../lib -I ../syntax

ifeq "${OCAMLDUCE}" "YES"
OCAMLDOC := ${OCAMLDUCEFIND} ocamldoc -package ocamlduce
else
OCAMLDOC := ${OCAMLFIND} ocamldoc
endif

doc: odoc

SYNTAX = -verbose -syntax camlp4o -package camlp4 -ppopt ../syntax/pa_include.cmo -ppopt o -ppopt -printer 

odoc:
	mkdir -p api-html
	$(OCAMLDOC) ${LIB} ${SYNTAX} -d api-html -intro indexdoc -html $(addprefix ../,${DOC})

clean:
	-rm -f api-html/*
distclean: clean
	-rm -f *~ \#* .\#*
