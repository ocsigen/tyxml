include ../Makefile.config
include ../Makefile.filelist

LIB := -I ../lib -I ../syntax

ifeq "${OCAMLDUCE}" "yes"
OCAMLDOC := ${OCAMLDUCEFIND} ocamldoc -package ocamlduce
ODOC_WIKI := odoc_duce_wiki.cma
else
OCAMLDOC := ${OCAMLFIND} ocamldoc
ODOC_WIKI := odoc_wiki.cma
endif

doc: odoc

odoc:
	mkdir -p api-html
	$(OCAMLDOC) ${LIB} -d api-html -intro indexdoc -html $(addprefix ../,${DOC})
odocwiki:
	mkdir -p api-wiki
	$(OCAMLDOC) ${LIB} -d api-wiki -intro indexdoc -colorize-code \
	   -i $(shell ocamlfind query wikidoc) -g ${ODOC_WIKI} \
	   $(addprefix ../,${DOC})

clean:
	-rm -f api-html/* api-wiki/*
distclean: clean
	-rm -f *~ \#* .\#*
