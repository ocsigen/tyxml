(*
   Copyright (C) 2004 by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
   Copyright (C) 2007 by Vincent Balat, Gabriel Kerneis
   Copyright (C) 2010 by Cecile Herbelin

   xHTML5.ml is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   xHTML5.ml is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *)

(** Typesafe constructors for HTML5 documents.
    @see <http://www.w3.org/TR/html5/> W3C Recommendation *)

module Make(XML : XML_defs.T)
           (SVG : SVG_defs.T with type raw_xml_elt = XML.elt
                              and type raw_xml_attrib = XML.attrib)
     : XHTML5_defs.T with type raw_xml_elt       = XML.elt
                      and type raw_xml_attrib    = XML.attrib
                      and type raw_xml_event     = XML.event
                      and type 'a raw_svg_elt    = 'a SVG.elt
                      and type 'a raw_svg_attrib = 'a SVG.attrib

module Make_05_00(XML : XML_defs.T)
                 (SVG : SVG_defs.T with type raw_xml_elt = XML.elt
                                    and type raw_xml_attrib = XML.attrib)
     : XHTML5_defs.T_05_00 with type raw_xml_elt       = XML.elt
                            and type raw_xml_attrib    = XML.attrib
                            and type raw_xml_event     = XML.event
                            and type 'a raw_svg_elt    = 'a SVG.elt
                            and type 'a raw_svg_attrib = 'a SVG.attrib
