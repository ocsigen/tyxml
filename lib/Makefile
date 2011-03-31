# $Id: Makefile,v 1.18 2005/06/20 17:57:58 ohl Exp $
#
# Copyright (C) 2004 by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
#
# XHTML is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# XHTML is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
########################################################################

include ../Makefile.config

FILES_F := uri.ml        \
	   XML_sigs.ml   \
	   XML_iter.ml   \
	   XML_print.ml  \
	   XHTML_sigs.ml \
	   XHTML_f.ml    \
	   SVG_sigs.ml   \
	   SVG_f.ml      \
	   HTML5_sigs.ml \
	   HTML5_f.ml    \

FILES := ${FILES_F} XML.ml XHTML.ml SVG.ml HTML5.ml

LIBS     := -package netstring

OCAMLC   := $(OCAMLFIND) ocamlc
OCAMLOPT := $(OCAMLFIND) ocamlopt
OCAMLDEP := $(OCAMLFIND) ocamldep

all: byte opt

byte: ${LIBNAME}.cma ${LIBNAME}_f.cma
opt: ${LIBNAME}.cmxa ${LIBNAME}_f.cmxa

${LIBNAME}.cma: $(FILES:.ml=.cmo)
	$(OCAMLC) -a $^ -o $@

${LIBNAME}.cmxa: $(FILES:.ml=.cmx)
	$(OCAMLOPT) -a $^ -o $@

${LIBNAME}_f.cma: $(FILES_F:.ml=.cmo)
	$(OCAMLC) -a $^ -o $@

${LIBNAME}_f.cmxa: $(FILES_F:.ml=.cmx)
	$(OCAMLOPT) -a $^ -o $@

##########

%.cmi: %.mli
	$(OCAMLC) ${LIBS} -c $<
%.cmo: %.ml
	$(OCAMLC) ${LIBS} -c $<
%.cmx: %.ml
	$(OCAMLOPT) ${LIBS} -c $<

# Clean up
clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
distclean: clean
	-rm -f *~ \#* .\#*

depend:
	$(OCAMLDEP) *.mli *.ml > .depend

FORCE:

-include .depend
