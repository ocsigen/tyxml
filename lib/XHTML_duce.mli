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

(** OCamlduce printers for XHTML 1.0 and 1.1 documents. *)

module type T = XML_sigs_duce.TypedXML
                with type doc = XHTML_types_duce.html
		 and type elt = {{ XHTML_types_duce.block
				 | XHTML_types_duce.form
				 | XHTML_types_duce.misc }}

module M : T

module P : XML_sigs_duce.TypedPrinter with module TypedXML := M
module P_01_00 : XML_sigs_duce.TypedPrinter with module TypedXML := M
