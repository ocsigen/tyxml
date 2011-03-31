(*
   Copyright (C) 2004 by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
   Copyright (C) 2007 by Vincent Balat, Gabriel Kerneis
   Copyright (C) 2010 by Cecile Herbelin

   HTML5.ml is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   HTML5.ml is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *)

(** Typesafe constructors for HTML5 documents.
    @see <http://www.w3.org/TR/html5/> W3C Recommendation *)

module Make(XML : XML_sigs.T)
           (SVG : SVG_sigs.SVG(XML).T)
     : HTML5_sigs.HTML5(XML)(SVG).T
