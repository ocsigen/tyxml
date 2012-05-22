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

(** Typesafe constructors for XHTML documents (Functorial interface) *)

module Make_01_00(X : Xml_sigs.T) : Xhtml_sigs.T_01_00 with type Xml.uri = X.uri
                                                       and type Xml.event_handler = X.event_handler
                                                       and type Xml.attrib = X.attrib
                                                       and type Xml.elt = X.elt

module Make_01_01(X : Xml_sigs.T) : Xhtml_sigs.T_01_01 with type Xml.uri = X.uri
                                                       and type Xml.event_handler = X.event_handler
                                                       and type Xml.attrib = X.attrib
                                                       and type Xml.elt = X.elt

module Make(X : Xml_sigs.T) : Xhtml_sigs.T with type Xml.uri = X.uri
                                           and type Xml.event_handler = X.event_handler
                                           and type Xml.attrib = X.attrib
                                           and type Xml.elt = X.elt

