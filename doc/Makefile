include ../Makefile.config
include ../Makefile.filelist

LIB := -I ../lib -I ../syntax

OCAMLDOC := ${OCAMLFIND} ocamldoc

doc: odoc

odoc:
	mkdir -p api-html
	$(OCAMLDOC) ${LIB} -d api-html -intro indexdoc -html $(addprefix ../,${DOC})

clean:
	-rm -f api-html/*
distclean: clean
	-rm -f *~ \#* .\#*
