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

let parse
    ~loc ~parent_lang
    ~name:((ns, name) as element_name) ~attributes children =

  let attributes = Attributes.parse loc element_name attributes in
  let lang, (module Reflected) = Namespace.reflect loc ns in

  let lang = match parent_lang, lang with
    | Common.Html, Svg -> Common.Html
    | Html, Html | Svg, Svg -> lang
    | Svg, Html ->
      Common.error loc
        "Nesting of Html element inside svg element is not supported"
  in

  let name =
    try List.assoc name Reflected.renamed_elements
    with Not_found -> Tyxml_name.ident name
  in
  let element_function = Common.make ~loc lang name in

  let assembler =
    try List.assoc name Reflected.element_assemblers
    with Not_found ->
      Common.error loc "Unknown %s element %s" (Common.lang lang) name
  in

  let children = assembler ~lang ~loc ~name children in

  Ast_helper.Exp.apply ~loc element_function (attributes @ children)

let comment ~loc ~lang s =
  let tot = Common.make ~loc lang "tot" in
  let comment = Common.make ~loc lang "Xml.comment" in
  let s = Common.string loc s in
  (* Using metaquot here avoids fiddling with labels. *)
  [%expr [%e tot] ([%e comment] [%e s])][@metaloc loc]
