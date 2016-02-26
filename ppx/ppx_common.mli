(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2016 Anton Bachin
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

val find : ('a -> bool) -> 'a list -> 'a option
(** Similar to [List.find], but evaluates to an option instead of raising
    [Not_found]. *)



(** Expression helpers. *)

val int_exp : Location.t -> int -> Parsetree.expression
val float_exp : Location.t -> string -> Parsetree.expression
val string_exp : Location.t -> string -> Parsetree.expression
val identifier : Location.t -> string -> Parsetree.expression
val list_exp : Location.t -> Parsetree.expression list -> Parsetree.expression

val wrap_exp :
  string -> Location.t -> Parsetree.expression -> Parsetree.expression
(** [wrap_exp implementation loc e] creates a parse tree for
    [implementation.Xml.W.return e]. *)



val error : Location.t -> ('b, unit, string, 'a) format4 -> 'b
(** Raises an error using compiler module [Location]. *)



val html5_implementation : string
(** The module name ["Html5"]. *)

val svg_implementation : string
(** The module name ["Svg"]. *)

val qualify : string -> string -> string
(** [qualify m i] is [m ^ "." ^ i]. *)
