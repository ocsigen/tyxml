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

let find f l =
  try Some (List.find f l)
  with Not_found -> None

let with_loc loc f x =
  with_default_loc loc @@ fun () -> f x

let int loc = with_loc loc Ast_convenience.int

let float loc = with_loc loc Ast_convenience.float

let string loc = with_loc loc Ast_convenience.str

let identifier loc s = Exp.ident ~loc (Location.mkloc (Longident.parse s) loc)

let list loc l =
  (l |> List.rev |> List.fold_left (fun acc tree ->
    [%expr [%e tree]::[%e acc]])
    [%expr []]) [@metaloc loc]

let error loc fmt = Location.raise_errorf ~loc ("Error: "^^fmt)

type lang = Html | Svg

let html5_implementation = "Html5"
let svg_implementation = "Svg"

let implementation = function
  | Html -> html5_implementation
  | Svg -> svg_implementation

let lang = function
  | Html -> "HTML"
  | Svg -> "SVG"

let qualify module_ identifier = Printf.sprintf "%s.%s" module_ identifier

let make ~loc i s = identifier loc (qualify (implementation i) s)

let wrap implementation loc e =
  [%expr
    [%e make ~loc implementation "Xml.W.return"]
    [%e e]] [@metaloc loc]
