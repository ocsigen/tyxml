(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2011 Pierre Chambart, Grégoire Henry
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Suite 500, Boston, MA 02111-1307, USA.
 *)

(** Typesafe constructors and printers for SVG documents.

    @see <http://www.w3.org/TR/SVG> W3C Recommendation *)

(** Concrete implementation of SVG typesafe constructors *)
module M : SVG_sigs.T with module XML := XML

(** Simple printer for SVG documents *)
module P : XML_sigs.TypedSimplePrinter with type 'a elt := 'a M.elt
				        and type doc := M.doc

(** Parametrized stream printer for SVG documents *)
module MakePrinter(O : XML_sigs.Output) :
  XML_sigs.TypedPrinter with type out := O.out
                         and type 'a elt := 'a M.elt
                         and type doc := M.doc

