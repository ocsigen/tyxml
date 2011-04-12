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

(** Type signature of XHTML typesafe constructors  *)
module type T = XHTML_sigs.XHTML(XML.M).T

(** Concrete implementation of XHTML typesafe constructors *)
module M : T

(** Simple printer for XHTML documents (HTML compatible printer) *)
module P : XML_sigs.TypedSimplePrinter(XML.M)(M).T

(** Parametrized stream printer for XHTML documents (HTML compatible printer) *)
module MakePrinter(O : XML_sigs.Output) : XML_sigs.TypedPrinter(XML.M)(M)(O).T

(** {1 XHTML 1.1} *)

(** Type signature of XHTML 1.1 typesafe constructors *)
module type T_01_01 = XHTML_sigs.XHTML(XML.M).T_01_01

(** Concrete implementation of XHTML 1.1 typesafe constructors *)
module M_01_01 : T_01_01

(** Simple printer for XHTML 1.1 documents *)
module P_01_01 : XML_sigs.TypedSimplePrinter(XML.M)(M_01_01).T

(** Simple printer for XHTML 1.1 documents (HTML compatible printer) *)
module P_01_01_compat : XML_sigs.TypedSimplePrinter(XML.M)(M_01_01).T

(** Parametrized stream printer for XHTML 1.1 documents *)
module MakePrinter_01_01(O : XML_sigs.Output) : XML_sigs.TypedPrinter(XML.M)(M_01_01)(O).T

(** Parametrized stream printer for XHTML 1.1 documents (HTML compatible printer) *)
module MakePrinter_01_01_compat(O : XML_sigs.Output) : XML_sigs.TypedPrinter(XML.M)(M_01_01)(O).T

(** {1 XHTML 1.0 } *)

(** Type signature of XHTML 1.0 typesafe constructors *)
module type T_01_00 = XHTML_sigs.XHTML(XML.M).T_01_00

(** Concrete implementation of XHTML 1.0 typesafe constructors *)
module M_01_00 : T_01_00

(** Simple printer for XHTML 1.0 documents *)
module P_01_00 : XML_sigs.TypedSimplePrinter(XML.M)(M_01_00).T

(** Simple printer for XHTML 1.0 documents (HTML compatible printer) *)
module P_01_00_compat : XML_sigs.TypedSimplePrinter(XML.M)(M_01_00).T

(** Parametrized stream printer for XHTML 1.0 documents *)
module MakePrinter_01_00(O : XML_sigs.Output) : XML_sigs.TypedPrinter(XML.M)(M_01_00)(O).T

(** Parametrized stream printer for XHTML 1.0 documents (HTML compatible printer) *)
module MakePrinter_01_00_compat(O : XML_sigs.Output) : XML_sigs.TypedPrinter(XML.M)(M_01_00)(O).T

(**/**)

module M_01_00_compat : T_01_00
module M_01_01_compat : T_01_01
