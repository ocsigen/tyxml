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
module M : Xhtml_sigs.T with type Xml.uri = Xml.uri
                        and type Xml.event_handler = Xml.event_handler
                        and type Xml.attrib = Xml.attrib
                        and type Xml.elt = Xml.elt

(** Simple printer for XHTML documents (HTML compatible printer) *)
module P : Xml_sigs.Typed_simple_printer with type 'a elt := 'a M.elt
					 and type doc := M.doc

(** Parametrized stream printer for XHTML documents (HTML compatible printer) *)
module Make_printer(O : Xml_sigs.Output) :
  Xml_sigs.Typed_printer with type out := O.out
			 and type 'a elt := 'a M.elt
			 and type doc := M.doc

(** {1 XHTML 1.1} *)

(** Concrete implementation of XHTML 1.1 typesafe constructors *)
module M_01_01 : Xhtml_sigs.T_01_01 with type Xml.uri = Xml.uri
                                    and type Xml.event_handler = Xml.event_handler
                                    and type Xml.attrib = Xml.attrib
                                    and type Xml.elt = Xml.elt

(** Simple printer for XHTML 1.1 documents (HTML compatible printer) *)
module P_01_01 :
  Xml_sigs.Typed_simple_printer with type 'a elt := 'a M_01_01.elt
                               and type doc := M_01_01.doc

(** Parametrized stream printer for XHTML 1.1 documents (HTML compatible printer) *)
module Make_printer_01_01(O : Xml_sigs.Output) :
  Xml_sigs.Typed_printer with type out := O.out
                         and type 'a elt := 'a M_01_01.elt
                         and type doc := M_01_01.doc

(** {1 XHTML 1.0 } *)

(** Concrete implementation of XHTML 1.0 typesafe constructors *)
module M_01_00 : Xhtml_sigs.T_01_00 with type Xml.uri = Xml.uri
                                    and type Xml.event_handler = Xml.event_handler
                                    and type Xml.attrib = Xml.attrib
                                    and type Xml.elt = Xml.elt


(** Simple printer for XHTML 1.0 documents (HTML compatible printer) *)
module P_01_00 :
  Xml_sigs.Typed_simple_printer with type 'a elt := 'a M_01_00.elt
                               and type doc := M_01_00.doc

(** Parametrized stream printer for XHTML 1.0 documents *)
module Make_printer_01_00(O : Xml_sigs.Output) :
  Xml_sigs.Typed_printer with type out := O.out
                         and type 'a elt := 'a M_01_00.elt
                         and type doc := M_01_00.doc
