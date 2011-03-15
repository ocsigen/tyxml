include ../Makefile.config
include ../Makefile.filelist

LIB := -I ../xhtml -I ../syntax -I ../xml-pretty

OCAMLDOC := ${OCAMLFIND} ocamldoc

doc: odoc

odoc:
	mkdir -p api-html
	$(OCAMLDOC) ${LIB} -d api-html -intro indexdoc -html $(addprefix ../,${DOC})

clean:
	-rm -f api-html/*
distclean: clean
	-rm -f *~ \#* .\#*
