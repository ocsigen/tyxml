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

(** Typesafe constructors and printers for XHTML 1.0 and 1.1 documents

    @see <http://www.w3.org/TR/xhtml11/> W3C Recommendation *)

(** {1 Current version } *)

(** Concrete implementation of XHTML typesafe constructors *)
module M : XHTML_sigs.T with module XML := XML

(** Simple printer for XHTML documents (HTML compatible printer) *)
module P : XML_sigs.TypedSimplePrinter with type 'a elt := 'a M.elt
					 and type doc := M.doc

(** Parametrized stream printer for XHTML documents (HTML compatible printer) *)
module MakePrinter(O : XML_sigs.Output) :
  XML_sigs.TypedPrinter with type out := O.out
			 and type 'a elt := 'a M.elt
			 and type doc := M.doc

(** {1 XHTML 1.1} *)

(** Concrete implementation of XHTML 1.1 typesafe constructors *)
module M_01_01 : XHTML_sigs.T_01_01 with module XML := XML

(** Simple printer for XHTML 1.1 documents *)
module P_01_01 :
  XML_sigs.TypedSimplePrinter with type 'a elt := 'a M_01_01.elt
                               and type doc := M_01_01.doc

(** Simple printer for XHTML 1.1 documents (HTML compatible printer) *)
module P_01_01_compat :
  XML_sigs.TypedSimplePrinter with type 'a elt := 'a M_01_01.elt
                               and type doc := M_01_01.doc

(** Parametrized stream printer for XHTML 1.1 documents *)
module MakePrinter_01_01(O : XML_sigs.Output) :
  XML_sigs.TypedPrinter with type out := O.out
                         and type 'a elt := 'a M_01_01.elt
                         and type doc := M_01_01.doc

(** Parametrized stream printer for XHTML 1.1 documents (HTML compatible printer) *)
module MakePrinter_01_01_compat(O : XML_sigs.Output) :
  XML_sigs.TypedPrinter with type out := O.out
                         and type 'a elt := 'a M_01_01.elt
                         and type doc := M_01_01.doc

(** {1 XHTML 1.0 } *)

(** Concrete implementation of XHTML 1.0 typesafe constructors *)
module M_01_00 : XHTML_sigs.T_01_00 with module XML := XML

(** Simple printer for XHTML 1.0 documents *)
module P_01_00 :
  XML_sigs.TypedSimplePrinter with type 'a elt := 'a M_01_00.elt
                               and type doc := M_01_00.doc

(** Simple printer for XHTML 1.0 documents (HTML compatible printer) *)
module P_01_00_compat :
  XML_sigs.TypedSimplePrinter with type 'a elt := 'a M_01_00.elt
			       and type doc := M_01_00.doc

(** Parametrized stream printer for XHTML 1.0 documents *)
module MakePrinter_01_00(O : XML_sigs.Output) :
  XML_sigs.TypedPrinter with type out := O.out
                         and type 'a elt := 'a M_01_00.elt
                         and type doc := M_01_00.doc


(** Parametrized stream printer for XHTML 1.0 documents (HTML compatible printer) *)
module MakePrinter_01_00_compat(O : XML_sigs.Output) :
  XML_sigs.TypedPrinter with type out := O.out
                         and type 'a elt := 'a M_01_00.elt
                         and type doc := M_01_00.doc

(**/**)

module M_01_00_compat : XHTML_sigs.T_01_00 with type 'a elt = 'a M_01_00.elt and module XML := XML
module M_01_01_compat : XHTML_sigs.T_01_01 with type 'a elt = 'a M_01_01.elt and module XML := XML

