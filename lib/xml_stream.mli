(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2018 Gabriel Radanne
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
 * Foundation, Inc., 51 Franklin Street, Suite 500, Boston, MA 02110-1301, USA.
*)

(** Streaming IO to/from XML trees *)

type name = string * string

(** {2 Input} *)

type signal = [
  | `Comment of string
  | `End_element
  | `Start_element of name * (name * string) list
  | `Text of string list
]

exception Malformed_stream

module Import (Xml : Xml_sigs.T) : sig
  val of_seq : signal Seq.t -> Xml.elt Xml.list_wrap
end

(** {2 Output} *)

type output = [ signal | `Raw of string list ]

module Typed_export
    (Xml : Xml_sigs.Iterable)
    (Typed_xml : Xml_sigs.Typed_xml with module Xml := Xml)
  : sig

    (** [export l] converts the Tyxml elements [l] into a signal.
        This signal is roughtly compatible with libraries to manipulate HTML
        and SVG such as Markup and Lambdasoup.
    *)
    val export : 'a Typed_xml.elt list -> output Seq.t
  end

module Export
    (Xml : Xml_sigs.Iterable)
  : sig
    val to_seq : ?namespace:string -> Xml.elt -> output Seq.t
    val to_seql : ?namespace:string -> Xml.elt list -> output Seq.t
  end
