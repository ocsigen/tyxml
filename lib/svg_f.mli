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

(** Typesafe constructors for SVG documents (Functorial interface) *)

(** This module defines basic data types for data, attributes
    and element occuring in SVG documents.
    It is based on the specification available at http://www.w3.org/TR/SVG/.

    This module is experimental, it may lack of some attributes,
    and the interface is very low level and do not take deeply into account
    the needs of SVG elements. *)

(*
open Svg_types
module Unit : sig

  open Unit

  val rel: float -> 'a quantity
      (** Do not specify the unit *)

  val deg : float -> angle
  val grad : float -> angle
  val rad : float -> angle

  val s : float -> time
  val ms : float -> time

  val em : float -> length
  val ex : float -> length
  val px : float -> length
  val in_ : float -> length
  val cm : float -> length
  val mm : float -> length
  val pt : float -> length
  val pc : float -> length

  val hz : float -> frequency
  val khz : float -> frequency

  val string_of_angle : angle -> string
  val string_of_time : time -> string
  val string_of_length : length -> string
  val string_of_freq : frequency -> string

end

open Unit

val string_of_number : number -> string
val string_of_number_optional_number : number_optional_number -> string
val string_of_percentage : percentage -> string
val string_of_strings : strings -> string
val string_of_spacestrings : spacestrings -> string
val string_of_commastrings : commastrings -> string
val string_of_fourfloats : fourfloats -> string
val string_of_numbers : numbers -> string
val string_of_numbers_semicolon : numbers_semicolon -> string
val string_of_lengths : lengths -> string
val string_of_coord : coord -> string
val string_of_coords : coords -> string
val string_of_transform : transform -> string
val string_of_transforms : transforms -> string
*)

module Make(Xml : Xml_sigs.Wrapped)
  : Svg_sigs.Make(Xml).T
    with type +'a elt = Xml.elt
     and type +'a attrib = Xml.attrib

(** Like the {! Svg_f.Make } functor, but allows to wrap elements inside a monad described by {! Xml_wrap.T}.
    See the functorial interface documentation for more details. *)
module MakeWrapped
    (W: Xml_wrap.T)
    (Xml : Xml_sigs.Wrapped with module W = W)
  : Svg_sigs.MakeWrapped(W)(Xml).T
    with type +'a elt = Xml.elt
     and type +'a attrib = Xml.attrib
