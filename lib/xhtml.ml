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

module M = Xhtml_f.Make(Xml)
module M_01_00 = Xhtml_f.Make_01_00(Xml)
module M_01_01 = Xhtml_f.Make_01_01(Xml)

module P = Xml_print.Make_typed_simple(Xml)(M)
module P_01_00 = Xml_print.Make_typed_simple(Xml)(M_01_00)
module P_01_01 = Xml_print.Make_typed_simple(Xml)(M_01_01)

module Make_printer = Xml_print.Make_typed(Xml)(M)
module Make_printer_01_00 = Xml_print.Make_typed(Xml)(M_01_00)
module Make_printer_01_01 = Xml_print.Make_typed(Xml)(M_01_01)
