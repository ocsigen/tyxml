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
open Parsetree



type assembler =
  string -> Location.t -> string -> Parsetree.expression list ->
    (Asttypes.label * Parsetree.expression) list



(* Helpers. *)

(* Called on a parse tree representing a child of an element. The argument
   [implementation] is the module name (string) ["Html5"] if the parent element
   is in the HTML namespace, and ["Svg"] if the parent is in the SVG namespace.

   - If the child is an unqualified application of the function [pcdata],
     qualifies it with the module [implementation].
   - If [implementation] is ["Html5"] and the child is an application of [svg]
     from any module, modifies the child to be an application of [Html5.svg]
   - Otherwise, evaluates to the child as passed. *)
let qualify_child implementation = function
  | [%expr pcdata [%e? s]] as e ->
    let identifier =
      Ppx_common.identifier e.pexp_loc
        (Ppx_common.qualify implementation "pcdata")
    in
    [%expr [%e identifier] [%e s]] [@metaloc e.pexp_loc]

  | {pexp_desc =
      Pexp_apply ({pexp_desc = Pexp_ident lid} as e', arguments)} as e
      when Longident.last lid.txt = "svg"
        && implementation = Ppx_common.html5_implementation ->
    let html5_svg = Ppx_common.qualify Ppx_common.html5_implementation "svg" in
    let lid = {lid with txt = Longident.parse html5_svg} in
    let identifier = {e' with pexp_desc = Pexp_ident lid} in
    {e with pexp_desc = Pexp_apply (identifier, arguments)}

  | e -> e

(* Called on a list of parse trees representing children of an element. The
   argument [implementation] is as in [_qualify_child]. Applies [_qualify_child]
   to each child, then assembles the children into a parse tree representing a
   value of type [_ implementation.list_wrap]. *)
let list_wrap_exp implementation loc es =
  let nil =
    [%expr
      [%e Ppx_common.identifier loc
        (Ppx_common.qualify implementation "Xml.W.nil")]
      ()] [@metaloc loc]
  in
  let cons =
    Ppx_common.identifier loc
      (Ppx_common.qualify implementation "Xml.W.cons")
  in

  es
  |> List.map (qualify_child implementation)
  |> List.rev
  |> List.fold_left (fun wrapped e ->
    [%expr [%e cons] [%e e] [%e wrapped]] [@metaloc loc])
    nil

(* Given a list of parse trees representing children of an element, filters out
   all children that consist of applications of [pcdata] to strings containing
   only whitespace. *)
let filter_whitespace children =
  children |> List.filter (function
    | [%expr pcdata [%e? {pexp_desc = Pexp_constant (Const_string (s, _))}]]
        when String.trim s = "" -> false
    | _ -> true)

(* Given a parse tree and a string [name], checks whether the parse tree is an
   application of a function with name [name]. *)
let is_element_with_name name = function
  | {pexp_desc = Pexp_apply ({pexp_desc = Pexp_ident {txt}}, _)}
      when Longident.flatten txt |> String.concat "." = name -> true
  | _ -> false

(* Partitions a list of elements according to [_is_element_with_name name]. *)
let partition name children =
  List.partition (is_element_with_name name) children

(* Given the name [n] of a function in [Html5_sigs.T], evaluates to
   ["Html5." ^ n]. *)
let html5 local_name =
  Ppx_common.qualify Ppx_common.html5_implementation local_name



(* Generic. *)

let nullary _ loc name children =
  if children <> [] then
    Ppx_common.error loc "%s should have no content" name;
  ["", [%expr ()] [@metaloc loc]]

let unary implementation loc name children =
  match children with
  | [child] ->
    let child =
      qualify_child implementation child
      |> Ppx_common.wrap_exp implementation loc
    in
    ["", child]
  | _ -> Ppx_common.error loc "%s should have exactly one child" name

let star implementation loc _ children =
  ["", list_wrap_exp implementation loc children]



(* Special-cased. *)

let html implementation loc name children =
  let children = filter_whitespace children in
  let head, others = partition (html5 "head") children in
  let body, others = partition (html5 "body") others in

  match head, body, others with
  | [head], [body], [] ->
    ["", Ppx_common.wrap_exp implementation loc head;
     "", Ppx_common.wrap_exp implementation loc body]
  | _ ->
    Ppx_common.error loc
      "%s element must have exactly head and body child elements" name

let head implementation loc name children =
  let title, others = partition (html5 "title") children in

  match title with
  | [title] ->
    ("", Ppx_common.wrap_exp implementation loc title)::
      (star implementation loc name others)
  | _ ->
    Ppx_common.error loc
      "%s element must have exactly one title child element" name

let figure implementation loc name children =
  begin match children with
  | [] -> star implementation loc name children
  | first::others ->
    if is_element_with_name (html5 "figcaption") first then
      ("figcaption",
       [%expr `Top [%e Ppx_common.wrap_exp implementation loc first]])::
          (star implementation loc name others)
    else
      let children_reversed = List.rev children in
      let last = List.hd children_reversed in
      if is_element_with_name (html5 "figcaption") last then
        let others = List.rev (List.tl children_reversed) in
        ("figcaption",
         [%expr `Bottom [%e Ppx_common.wrap_exp implementation loc last]])::
            (star implementation loc name others)
      else
        star implementation loc name children
  end [@metaloc loc]

let object_ implementation loc name children =
  let params, others = partition (html5 "param") children in

  if params <> [] then
    ("params", list_wrap_exp implementation loc params)::
      (star implementation loc name others)
  else
    star implementation loc name others

let audio_video implementation loc name children =
  let sources, others = partition (html5 "source") children in

  if sources <> [] then
    ("srcs", list_wrap_exp implementation loc sources)::
      (star implementation loc name others)
  else
    star implementation loc name others

let table implementation loc name children =
  let caption, others = partition (html5 "caption") children in
  let columns, others = partition (html5 "colgroup") others in
  let thead, others = partition (html5 "thead") others in
  let tfoot, others = partition (html5 "tfoot") others in

  let one label = function
    | [] -> []
    | [child] -> [label, Ppx_common.wrap_exp implementation loc child]
    | _ -> Ppx_common.error loc "%s cannot have more than one %s" name label
  in

  let columns =
    if columns = [] then []
    else ["columns", list_wrap_exp implementation loc columns]
  in

  (one "caption" caption) @
    columns @
    (one "thead" thead) @
    (one "tfoot" tfoot) @
    (star implementation loc name others)

let fieldset implementation loc name children =
  let legend, others = partition (html5 "legend") children in

  match legend with
  | [] -> star implementation loc name others
  | [legend] ->
    ("legend", Ppx_common.wrap_exp implementation loc legend)::
      (star implementation loc name others)
  | _ -> Ppx_common.error loc "%s cannot have more than one legend" name

let datalist implementation loc name children =
  let options, others = partition (html5 "option") children in

  let children =
    begin match others with
    | [] ->
      "children",
      [%expr `Options [%e list_wrap_exp implementation loc options]]

    | _ ->
      "children",
      [%expr `Phras [%e list_wrap_exp implementation loc children]]
    end [@metaloc loc]
  in

  children::(nullary implementation loc name [])

let details implementation loc name children =
  let summary, others = partition (html5 "summary") children in

  match summary with
  | [summary] ->
    ("", Ppx_common.wrap_exp implementation loc summary)::
      (star implementation loc name others)
  | _ -> Ppx_common.error loc "%s must have exactly one summary child" name

let menu implementation loc name children =
  let children =
    "child",
    [%expr `Flows [%e list_wrap_exp implementation loc children]]
      [@metaloc loc]
  in
  children::(nullary implementation loc name [])
