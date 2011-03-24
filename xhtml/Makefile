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

FILES_F := uri.ml    \
	   svgtypes.ml SVG_f.ml \
	   XHTML5_f.ml \
	   XHTML_f.ml \

FILES := ${FILES_F} XML.ml SVG.ml XHTML5.ml XHTML.ml

LIBS     := -package netstring

OCAMLC   := $(OCAMLFIND) ocamlc
OCAMLOPT := $(OCAMLFIND) ocamlopt
OCAMLDEP := $(OCAMLFIND) ocamldep

all: byte opt

byte: xhtml.cma xhtml_f.cma
opt: xhtml.cmxa xhtml_f.cmxa

xhtml.cma: $(FILES:.ml=.cmo)
	$(OCAMLC) -a $^ -o $@

xhtml.cmxa: $(FILES:.ml=.cmx)
	$(OCAMLOPT) -a $^ -o $@

xhtml_f.cma: $(FILES_F:.ml=.cmo)
	$(OCAMLC) -a $^ -o $@

xhtml_f.cmxa: $(FILES_F:.ml=.cmx)
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
