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


(** Markup language *)

type lang = Html | Svg
val lang : lang -> string
val implementation : lang -> string

val make :
  loc:Location.t -> lang -> string -> Parsetree.expression

(** Expression helpers. *)

val int : Location.t -> int -> Parsetree.expression
val float : Location.t -> string -> Parsetree.expression
val string : Location.t -> string -> Parsetree.expression
val list : Location.t -> Parsetree.expression list -> Parsetree.expression

val wrap :
  lang -> Location.t -> Parsetree.expression -> Parsetree.expression
(** [wrap_exp implementation loc e] creates a parse tree for
    [implementation.Xml.W.return e]. *)

val error : Location.t -> ('b, unit, string, 'a) format4 -> 'b
(** Raises an error using compiler module [Location]. *)
