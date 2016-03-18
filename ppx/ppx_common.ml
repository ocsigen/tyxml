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

open Asttypes
open Ast_helper

let find f l =
  try Some (List.find f l)
  with Not_found -> None

let int_exp loc n = Exp.constant ~loc (Const_int n)

let float_exp loc s = Exp.constant ~loc (Const_float s)

let string_exp loc s = Exp.constant ~loc (Const_string (s, None))

let identifier loc s = Exp.ident ~loc (Location.mkloc (Longident.parse s) loc)

let list_exp loc l =
  (l |> List.rev |> List.fold_left (fun acc tree ->
    [%expr [%e tree]::[%e acc]])
    [%expr []]) [@metaloc loc]

let error loc fmt = Location.raise_errorf ~loc ("Error: "^^fmt)

let html5_implementation = "Html5"
let svg_implementation = "Svg"

let qualify module_ identifier = Printf.sprintf "%s.%s" module_ identifier

let wrap_exp implementation loc e =
  [%expr
    [%e identifier loc (qualify implementation "Xml.W.return")]
    [%e e]] [@metaloc loc]
