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
  Parsetree.expression Ppx_common.value list ->
  (Pc.Label.t * Parsetree.expression) list



(* Helpers. *)

(* Given a parse tree [e], if [e] represents [_.pcdata s], where [s] is a string
   constant, evaluates to [Some s]. Otherwise, evaluates to [None]. *)
let to_pcdata = function
  | [%expr [%e? {pexp_desc = Pexp_ident f; _}] [%e? arg]] -> begin
      match Longident.last f.txt, Ast_convenience.get_str arg with
      | "pcdata", Some s -> Some s
      | _ -> None
    end
  | _ -> None

(** Test if the expression is a pcdata containing only whitespaces. *)
let is_whitespace = function
  | Pc.Val e -> begin
      match to_pcdata e with
      | Some s when String.trim s = "" -> true
      | _ -> false
    end
  | _ -> false

(* Given a list of parse trees representing children of an element, filters out
   all children that consist of applications of [pcdata] to strings containing
   only whitespace. *)
let filter_whitespace = List.filter (fun e -> not @@ is_whitespace e)

let filter_surrounding_whitespace children =
  let rec aux = function
    | [] -> []
    | h :: t when is_whitespace h -> aux t
    | l -> List.rev l
  in
  aux @@ aux children

(* Given a parse tree and a string [name], checks whether the parse tree is an
   application of a function with name [name]. *)
let is_element_with_name name = function
  | Pc.Val {pexp_desc = Pexp_apply ({pexp_desc = Pexp_ident {txt}}, _)}
      when txt = name -> true
  | _ -> false

(* Partitions a list of elements according to [is_element_with_name name]. *)
let partition name children =
  List.partition (is_element_with_name name) children

(* Given the name [n] of a function in [Html_sigs.T], evaluates to
   ["Html." ^ n]. *)
let html local_name =
  Longident.Ldot (Lident Pc.(implementation Html), local_name)



(* Generic. *)

let nullary ~lang:_ ~loc ~name children =
  if children <> [] then
    Pc.error loc "%s should have no content" name;
  [Pc.Label.nolabel, [%expr ()] [@metaloc loc]]

let unary ~lang ~loc ~name children =
  match children with
  | [child] ->
    let child = Pc.wrap_value lang loc child in
    [Pc.Label.nolabel, child]
  | _ -> Pc.error loc "%s should have exactly one child" name

let star ~lang ~loc ~name:_ children =
  [Pc.Label.nolabel, Pc.list_wrap_value lang loc children]



(* Special-cased. *)

let ul ~lang ~loc ~name children =
  let children = filter_whitespace children in
  star ~lang ~loc ~name children

let ol ~lang ~loc ~name children =
  let children = filter_whitespace children in
  star ~lang ~loc ~name children

let head ~lang ~loc ~name children =
  let children = filter_whitespace children in
  let title, others = partition (html "title") children in

  match title with
  | [title] ->
    (Pc.Label.nolabel, Pc.wrap_value lang loc title) :: star ~lang ~loc ~name others
  | _ ->
    Pc.error loc
      "%s element must have exactly one title child element" name

let figure ~lang ~loc ~name children =
  begin match children with
  | [] -> star ~lang ~loc ~name children
  | first::others ->
    if is_element_with_name (html "figcaption") first then
      (Pc.Label.labelled "figcaption",
       [%expr `Top [%e Pc.wrap_value lang loc first]])::
          (star ~lang ~loc ~name others)
    else
      let children_reversed = List.rev children in
      let last = List.hd children_reversed in
      if is_element_with_name (html "figcaption") last then
        let others = List.rev (List.tl children_reversed) in
        (Pc.Label.labelled "figcaption",
         [%expr `Bottom [%e Pc.wrap_value lang loc last]])::
            (star ~lang ~loc ~name others)
      else
        star ~lang ~loc ~name children
  end [@metaloc loc]

let object_ ~lang ~loc ~name children =
  let params, others = partition (html "param") children in

  if params <> [] then
    (Pc.Label.labelled "params", Pc.list_wrap_value lang loc params) ::
    star ~lang ~loc ~name others
  else
    star ~lang ~loc ~name others

let audio_video ~lang ~loc ~name children =
  let sources, others = partition (html "source") children in

  if sources <> [] then
    (Pc.Label.labelled "srcs", Pc.list_wrap_value lang loc sources) ::
    star ~lang ~loc ~name others
  else
    star ~lang ~loc ~name others

let table ~lang ~loc ~name children =
  let caption, others = partition (html "caption") children in
  let columns, others = partition (html "colgroup") others in
  let thead, others = partition (html "thead") others in
  let tfoot, others = partition (html "tfoot") others in

  let one label = function
    | [] -> []
    | [child] -> [Pc.Label.labelled label, Pc.wrap_value lang loc child]
    | _ -> Pc.error loc "%s cannot have more than one %s" name label
  in

  let columns =
    if columns = [] then []
    else [Pc.Label.labelled "columns", Pc.list_wrap_value lang loc columns]
  in

  (one "caption" caption) @
    columns @
    (one "thead" thead) @
    (one "tfoot" tfoot) @
    (star ~lang ~loc ~name others)

let fieldset ~lang ~loc ~name children =
  let legend, others = partition (html "legend") children in

  match legend with
  | [] -> star ~lang ~loc ~name others
  | [legend] ->
    (Pc.Label.labelled "legend", Pc.wrap_value lang loc legend)::
      (star ~lang ~loc ~name others)
  | _ -> Pc.error loc "%s cannot have more than one legend" name

let datalist ~lang ~loc ~name children =
  let options, others = partition (html "option") children in

  let children =
    begin match others with
    | [] ->
      Pc.Label.labelled "children",
      [%expr `Options [%e Pc.list_wrap_value lang loc options]]

    | _ ->
      Pc.Label.labelled "children",
      [%expr `Phras [%e Pc.list_wrap_value lang loc children]]
    end [@metaloc loc]
  in

  children::(nullary ~lang ~loc ~name [])

let details ~lang ~loc ~name children =
  let summary, others = partition (html "summary") children in

  match summary with
  | [summary] ->
    (Pc.Label.nolabel, Pc.wrap_value lang loc summary)::
      (star ~lang ~loc ~name others)
  | _ -> Pc.error loc "%s must have exactly one summary child" name

let menu ~lang ~loc ~name children =
  let children =
    Pc.Label.labelled "child",
    [%expr `Flows [%e Pc.list_wrap_value lang loc children]]
      [@metaloc loc]
  in
  children::(nullary ~lang ~loc ~name [])

let html ~lang ~loc ~name children =
  let children = filter_whitespace children in
  let head, others = partition (html "head") children in
  let body, others = partition (html "body") others in

  match head, body, others with
  | [head], [body], [] ->
    [Pc.Label.nolabel, Pc.wrap_value lang loc head;
     Pc.Label.nolabel, Pc.wrap_value lang loc body]
  | _ ->
    Pc.error loc
      "%s element must have exactly head and body child elements" name
