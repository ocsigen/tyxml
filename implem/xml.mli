(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2004 Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
 * Copyright (C) 2007 Gabriel Kerneis
 * Copyright (C) 2010 Cecile Herbelin
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

(** Basic functions for construction and manipulation of XML tree. *)

type 'a wrap = 'a
type 'a list_wrap = 'a list
module W : Xml_wrap.NoWrap
type uri = string
val string_of_uri : uri -> string
val uri_of_string : string -> uri

type aname = string
type separator = Space | Comma
type event_handler = string
type mouse_event_handler = string
type keyboard_event_handler = string

type attrib
val aname : attrib -> aname

type acontent = private
  | AFloat of float
  | AInt of int
  | AStr of string
  | AStrL of separator * string list
val acontent : attrib -> acontent

val float_attrib : aname -> float -> attrib
val int_attrib : aname -> int -> attrib
val string_attrib : aname -> string -> attrib
val space_sep_attrib : aname -> string list -> attrib
val comma_sep_attrib : aname -> string list -> attrib
val event_handler_attrib : aname -> event_handler -> attrib
val mouse_event_handler_attrib : aname -> event_handler -> attrib
val keyboard_event_handler_attrib : aname -> event_handler -> attrib
val uri_attrib : aname -> uri -> attrib
val uris_attrib : aname -> uri list -> attrib

type ename = string

type elt
type econtent = private
  | Empty
  | Comment of string
  | EncodedPCDATA of string
  | PCDATA of string
  | Entity of string
  | Leaf of ename * attrib list
  | Node of ename * attrib list * elt list
val content : elt -> econtent

val empty : unit -> elt

val comment : string -> elt
val pcdata : string -> elt
val encodedpcdata : string -> elt
val entity : string -> elt
(** Neither [comment], [pcdata] nor [entity] check their argument for invalid
    characters.  Unsafe characters will be escaped later by the output routines.  *)

val leaf : ?a:(attrib list) -> ename -> elt
val node : ?a:(attrib list) -> ename -> elt list -> elt

val cdata : string -> elt
val cdata_script : string -> elt
val cdata_style : string -> elt

(** {2 Iterators} *)

val amap : (ename -> attrib list -> attrib list) -> elt -> elt
(** Recursively edit attributes for the element and all its children. *)

val amap1 : (ename -> attrib list -> attrib list) -> elt -> elt
(** Edit attributes only for one element. *)

(** The following can safely be exported by higher level libraries,
    because removing an attribute from a element is always legal. *)

val rm_attrib : (aname -> bool) -> attrib list -> attrib list
val rm_attrib_from_list : (aname -> bool) -> (string -> bool) -> attrib list -> attrib list

val map_int_attrib :
  (aname -> bool) -> (int -> int) -> attrib list -> attrib list
val map_string_attrib :
  (aname -> bool) -> (string -> string) -> attrib list -> attrib list
val map_string_attrib_in_list :
  (aname -> bool) -> (string -> string) -> attrib list -> attrib list

(** Exporting the following by higher level libraries would drive
    a hole through a type system, because they allow to add {e any}
    attribute to {e any} element. *)

val add_int_attrib : aname -> int -> attrib list -> attrib list
val add_string_attrib : aname -> string -> attrib list -> attrib list
val add_comma_sep_attrib : aname -> string -> attrib list -> attrib list
val add_space_sep_attrib : aname -> string -> attrib list -> attrib list

val fold : (unit -> 'a) -> (string -> 'a) -> (string -> 'a) -> (string -> 'a) ->
  (string -> 'a) -> (ename -> attrib list -> 'a) ->
  (ename -> attrib list -> 'a list -> 'a) ->
  elt -> 'a

val all_entities : elt -> string list

val translate :
  (ename -> attrib list -> elt) ->
  (ename -> attrib list -> elt list -> elt) ->
  ('state -> ename -> attrib list -> elt list) ->
  ('state -> ename -> attrib list -> elt list -> elt list) ->
  (ename -> attrib list -> 'state -> 'state) -> 'state -> elt -> elt

(** {2 Printer} *)

val print_list:
  output:(string -> unit) -> ?encode:(string -> string) -> elt list -> unit

val print : Format.formatter -> elt -> unit
