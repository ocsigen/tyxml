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

open Ast_helper
module Label = Ast_convenience.Label

(** Lang utilities *)

type lang = Html | Svg

let html5_implementation = ref "Html5"
let svg_implementation = ref "Svg"

let implemenentation_ref = function
  | Html -> html5_implementation
  | Svg -> svg_implementation

let set_implementation lang s =
  (implemenentation_ref lang) := s

let implementation lang =
  !(implemenentation_ref lang)

let lang = function
  | Html -> "HTML"
  | Svg -> "SVG"

let make ~loc i s =
  let lid = Longident.parse @@ implementation i ^ "." ^ s in
  Exp.ident ~loc @@ Location.mkloc lid loc

(** Generic *)

let find f l =
  try Some (List.find f l)
  with Not_found -> None

let with_loc loc f x =
  with_default_loc loc @@ fun () -> f x
let error loc fmt = Location.raise_errorf ~loc ("Error: "^^fmt)

(** Ast manipulation *)

let int loc = with_loc loc Ast_convenience.int

let float loc = with_loc loc Ast_convenience.float

let string loc = with_loc loc Ast_convenience.str

let list_gen cons nil l =
  (l |> List.rev |> List.fold_left cons nil)

let list loc =
  let nil = [%expr []][@metaloc loc] in
  let cons acc x = [%expr [%e x]::[%e acc]][@metaloc loc] in
  list_gen cons nil

let list_wrap lang loc =
  let nil =
    [%expr
      [%e make ~loc lang "Xml.W.nil"]
      ()] [@metaloc loc]
  in
  let cons acc x =
    [%expr [%e make ~loc lang "Xml.W.cons"] [%e x] [%e acc]][@metaloc loc]
  in
  list_gen cons nil

let wrap implementation loc e =
  [%expr
    [%e make ~loc implementation "Xml.W.return"]
    [%e e]] [@metaloc loc]
