(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2016 Anton Bachin, Gabriel Radanne
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

(** Internal functions to build tyxml's ppx. *)

val markup_to_expr :
  ?context:[ `Document | `Fragment of string ] ->
  Location.t -> Parsetree.expression -> Parsetree.expression
(** Given the payload of a [%html5 ...] or [%svg ...] expression,
    converts it to a TyXML expression representing the markup
    contained therein. *)

val mapper : string list -> Ast_mapper.mapper
