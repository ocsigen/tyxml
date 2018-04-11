BLD=../_build/default
SRC=..

PKGS=\
  -package uchar -package uutf -package re -package markup \
  -package ppx_tools_versioned

INCS=\
	-I ${BLD}/lib/.*.objs \
	-I ${BLD}/implem/.*.objs \
	-I ${BLD}/tools/.*.objs \
	-I ${BLD}/tools/.*.objs

MLIS=\
  $(wildcard ${SRC}/lib/*.mli) \
	$(wildcard ${SRC}/implem/*.mli) ${SRC}/implem/tyxml.ml \
	$(wildcard ${SRC}/tools/*.mli) \
  ${SRC}/ppx/ppx_tyxml.mli

DOCOPT := -colorize-code -short-functors -charset utf-8

.PHONY: doc wikidoc
doc: api/html/index.html
api/html/index.html: ${MLIS} indexdoc
	mkdir -p api/html
	ocamlfind ocamldoc ${DOCOPT} ${PKGS} ${INCS} -intro indexdoc -html \
		-d api/html \
		${MLIS}

wikidoc: api/wiki/index.wiki
api/wiki/index.wiki: ${MLIS} indexdoc
	mkdir -p api/wiki
	ocamlfind ocamldoc ${DOCOPT} ${PKGS} ${INCS} -intro indexdoc \
		-d api/wiki \
		-i $(shell ocamlfind query wikidoc) -g odoc_wiki.cma \
		${MLIS}

clear:
	rm -rf api
