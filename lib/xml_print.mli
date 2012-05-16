(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2008 Vincent Balat, Mauricio Fernandez
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

(** Printer for XML. *)

val encode_unsafe_char : string -> string
(** The encoder maps strings to HTML and {e must} encode the unsafe characters
    ['<'], ['>'], ['"'], ['&'] and the control characters 0-8, 11-12, 14-31, 127
    to HTML entities.  [encode_unsafe] is the default for [?encode] in [output]
    and [pretty_print] below.  Other implementations are provided by the module
    [Netencoding] in the
    {{:http://www.ocaml-programming.de/programming/ocamlnet.html}OcamlNet}
    library, e.g.:
    [let encode = Netencoding.Html.encode ~in_enc:`Enc_iso88591 ~out_enc:`Enc_usascii ()],
    Where national characters are replaced by HTML entities.
    The user is of course free to write her own implementation.
    @see <http://www.ocaml-programming.de/programming/ocamlnet.html> OcamlNet *)

val encode_unsafe_char_and_at : string -> string
(** In addition, encode ["@"] as ["&#64;"] in the hope that this will fool
    simple minded email address harvesters. *)

val compose_decl : ?version:string -> ?encoding:string -> unit -> string
(** [encoding] is the name of the character encoding, e.g. ["US-ASCII"] or ["UTF-8"] *)

val compose_doctype : string -> string list -> string

module Make(Xml : Xml_sigs.Iterable)
           (I : sig val emptytags : string list end)
           (O : Xml_sigs.Output)
     : Xml_sigs.Printer with type out := O.out and type xml_elt := Xml.elt

module Make_typed(Xml : Xml_sigs.Iterable)
                (Typed_xml : Xml_sigs.Typed_xml with module Xml := Xml)
                (O : Xml_sigs.Output)
     : Xml_sigs.Typed_printer with type out := O.out
                              and type 'a elt := 'a Typed_xml.elt
                              and type doc := Typed_xml.doc

module Make_simple(Xml : Xml_sigs.Iterable)(F : sig val emptytags : string list end)
     : Xml_sigs.Simple_printer with type xml_elt := Xml.elt

module Make_typed_simple(Xml : Xml_sigs.Iterable)
                      (Typed_xml : Xml_sigs.Typed_xml with  module Xml := Xml)
     : Xml_sigs.Typed_simple_printer with type 'a elt := 'a Typed_xml.elt
                                    and type doc := Typed_xml.doc
