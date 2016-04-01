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
module Pc = Ppx_common

type assembler =
  lang:Ppx_common.lang ->
  loc:Location.t ->
  name:string ->
  Parsetree.expression list ->
  (Pc.Label.t * Parsetree.expression) list



(* Helpers. *)

(* Called on a parse tree representing a child of an element. The argument
   [implementation] is the module name (string) ["Html5"] if the parent element
   is in the HTML namespace, and ["Svg"] if the parent is in the SVG namespace.

   - If the child is an unqualified application of the function [pcdata],
     qualifies it with the module [implementation].
   - If [implementation] is ["Html5"] and the child is an application of [svg]
     from any module, modifies the child to be an application of [Html5.svg]
   - Otherwise, evaluates to the child as passed. *)
let qualify_child lang = function
  | [%expr pcdata [%e? s]] as e ->
    let identifier =
      Pc.make ~loc:e.pexp_loc lang "pcdata"
    in
    [%expr [%e identifier] [%e s]] [@metaloc e.pexp_loc]

  | {pexp_desc =
      Pexp_apply ({pexp_desc = Pexp_ident lid}, arguments)} as e
      when Longident.last lid.txt = "svg" && lang = Html ->
    let identifier = Pc.make ~loc:lid.loc Html "svg" in
    {e with pexp_desc = Pexp_apply (identifier, arguments)}

  | e -> e

(* Called on a list of parse trees representing children of an element. The
   argument [implementation] is as in [qualify_child]. Applies [qualify_child]
   to each child, then assembles the children into a parse tree representing a
   value of type [_ implementation.list_wrap]. *)
let list_wrap_exp lang loc es =
  es
  |> List.map (qualify_child lang)
  |> Pc.list_wrap lang loc

(* Given a list of parse trees representing children of an element, filters out
   all children that consist of applications of [pcdata] to strings containing
   only whitespace. *)
let filter_whitespace children =
  children |> List.filter (function
    | [%expr pcdata [%e? s]]-> begin
        match Ast_convenience.get_str s with
        | Some s when String.trim s = "" -> false
        | _ -> true
      end
    | _ -> true)

(* Given a parse tree and a string [name], checks whether the parse tree is an
   application of a function with name [name]. *)
let is_element_with_name name = function
  | {pexp_desc = Pexp_apply ({pexp_desc = Pexp_ident {txt}}, _)}
      when txt = name -> true
  | _ -> false

(* Partitions a list of elements according to [is_element_with_name name]. *)
let partition name children =
  List.partition (is_element_with_name name) children

(* Given the name [n] of a function in [Html5_sigs.T], evaluates to
   ["Html5." ^ n]. *)
let html5 local_name =
  Longident.Ldot (Lident Pc.(implementation Html), local_name)



(* Generic. *)

let nullary ~lang:_ ~loc ~name children =
  if children <> [] then
    Pc.error loc "%s should have no content" name;
  [Pc.Label.nolabel, [%expr ()] [@metaloc loc]]

let unary ~lang ~loc ~name children =
  match children with
  | [child] ->
    let child =
      qualify_child lang child
      |> Pc.wrap lang loc
    in
    [Pc.Label.nolabel, child]
  | _ -> Pc.error loc "%s should have exactly one child" name

let star ~lang ~loc ~name:_ children =
  [Pc.Label.nolabel, list_wrap_exp lang loc children]



(* Special-cased. *)

let html ~lang ~loc ~name children =
  let children = filter_whitespace children in
  let head, others = partition (html5 "head") children in
  let body, others = partition (html5 "body") others in

  match head, body, others with
  | [head], [body], [] ->
    [Pc.Label.nolabel, Pc.wrap lang loc head;
     Pc.Label.nolabel, Pc.wrap lang loc body]
  | _ ->
    Pc.error loc
      "%s element must have exactly head and body child elements" name

let head ~lang ~loc ~name children =
  let title, others = partition (html5 "title") children in

  match title with
  | [title] ->
    (Pc.Label.nolabel, Pc.wrap lang loc title) :: star ~lang ~loc ~name others
  | _ ->
    Pc.error loc
      "%s element must have exactly one title child element" name

let figure ~lang ~loc ~name children =
  begin match children with
  | [] -> star ~lang ~loc ~name children
  | first::others ->
    if is_element_with_name (html5 "figcaption") first then
      ("figcaption",
       [%expr `Top [%e Pc.wrap lang loc first]])::
          (star ~lang ~loc ~name others)
    else
      let children_reversed = List.rev children in
      let last = List.hd children_reversed in
      if is_element_with_name (html5 "figcaption") last then
        let others = List.rev (List.tl children_reversed) in
        ("figcaption",
         [%expr `Bottom [%e Pc.wrap lang loc last]])::
            (star ~lang ~loc ~name others)
      else
        star ~lang ~loc ~name children
  end [@metaloc loc]

let object_ ~lang ~loc ~name children =
  let params, others = partition (html5 "param") children in

  if params <> [] then
    ("params", list_wrap_exp lang loc params) :: star ~lang ~loc ~name others
  else
    star ~lang ~loc ~name others

let audio_video ~lang ~loc ~name children =
  let sources, others = partition (html5 "source") children in

  if sources <> [] then
    ("srcs", list_wrap_exp lang loc sources) :: star ~lang ~loc ~name others
  else
    star ~lang ~loc ~name others

let table ~lang ~loc ~name children =
  let caption, others = partition (html5 "caption") children in
  let columns, others = partition (html5 "colgroup") others in
  let thead, others = partition (html5 "thead") others in
  let tfoot, others = partition (html5 "tfoot") others in

  let one label = function
    | [] -> []
    | [child] -> [label, Pc.wrap lang loc child]
    | _ -> Pc.error loc "%s cannot have more than one %s" name label
  in

  let columns =
    if columns = [] then []
    else ["columns", list_wrap_exp lang loc columns]
  in

  (one "caption" caption) @
    columns @
    (one "thead" thead) @
    (one "tfoot" tfoot) @
    (star ~lang ~loc ~name others)

let fieldset ~lang ~loc ~name children =
  let legend, others = partition (html5 "legend") children in

  match legend with
  | [] -> star ~lang ~loc ~name others
  | [legend] ->
    ("legend", Pc.wrap lang loc legend)::
      (star ~lang ~loc ~name others)
  | _ -> Pc.error loc "%s cannot have more than one legend" name

let datalist ~lang ~loc ~name children =
  let options, others = partition (html5 "option") children in

  let children =
    begin match others with
    | [] ->
      "children",
      [%expr `Options [%e list_wrap_exp lang loc options]]

    | _ ->
      "children",
      [%expr `Phras [%e list_wrap_exp lang loc children]]
    end [@metaloc loc]
  in

  children::(nullary ~lang ~loc ~name [])

let details ~lang ~loc ~name children =
  let summary, others = partition (html5 "summary") children in

  match summary with
  | [summary] ->
    (Pc.Label.nolabel, Pc.wrap lang loc summary)::
      (star ~lang ~loc ~name others)
  | _ -> Pc.error loc "%s must have exactly one summary child" name

let menu ~lang ~loc ~name children =
  let children =
    "child",
    [%expr `Flows [%e list_wrap_exp lang loc children]]
      [@metaloc loc]
  in
  children::(nullary ~lang ~loc ~name [])
