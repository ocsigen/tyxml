(* $Id: xML.mli,v 1.15 2004/12/13 14:57:45 ohl Exp $

   Copyright (C) 2004 by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>

   XHTML is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   XHTML is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *)

type aname = string
type separator = Space | Comma
type event = string

type attrib
val aname : attrib -> aname

type acontent = private
  | AFloat of aname * float
  | AInt of aname * int
  | AStr of aname * string
  | AStrL of separator * aname * string list
val acontent : attrib -> acontent

val float_attrib : aname -> float -> attrib
val int_attrib : aname -> int -> attrib
val string_attrib : aname -> string -> attrib
val space_sep_attrib : aname -> string list -> attrib
val comma_sep_attrib : aname -> string list -> attrib
val event_attrib : aname -> event -> attrib

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
(** NB: [Leaf ("foo", []) -> "<foo />"], but [Node ("foo", [], []) -> "<foo></foo>"] *)

val cdata : string -> elt
val cdata_script : string -> elt
val cdata_style : string -> elt
