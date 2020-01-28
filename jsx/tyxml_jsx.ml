open Ast_mapper
open Parsetree
open Asttypes

open Tyxml_syntax

let to_kebab_case name =
  let length = String.length name in
  if length > 4 then
    let first = String.sub name 0 4 in
    match first with
    | "aria"
    | "data" ->
      first ^ "-" ^ String.lowercase_ascii (String.sub name 4 (length - 1))
    | _ -> name
  else
    name

let make_html_attr_name name =
  let name =
    match name with
    | "class_" -> "class"
    | "className" -> "class"
    | "htmlFor" -> "for"
    | "type_" -> "type"
    | "to_" -> "to"
    | "open_" -> "open"
    | "begin_" -> "begin"
    | "end_" -> "end"
    | "in_" -> "in"
    | "method_" -> "method"
    | name -> to_kebab_case name
  in
  Common.Html, name

open Common

let rec filter_map f = function
  | [] -> []
  | a :: q ->
  match f a with
  | None -> filter_map f q
  | Some a -> a :: filter_map f q

type attr = {
  a_name: Common.name;
  a_value : string value;
  a_loc: Location.t;
}

let rec map_element_children mapper elements =
  let rec map acc e =
    match e with
    | [%expr []] -> List.rev acc
    | [%expr [%e? child] :: [%e? rest]] -> map (Val (children_mapper mapper child) :: acc) rest
    | e -> List.rev ((Antiquot e) :: acc)
  in
  map [] elements

and jsx_args_to_tyxml_args mapper args =
  try
    let _, children =
      List.find
        (function
          | Labelled "children", _ -> true
          | _ -> false)
        args
    in
    map_element_children mapper children
  with Not_found -> []

and extract_attr_value a_name a_value =
  let a_name = make_html_attr_name a_name in
  match a_value with
  | { pexp_desc = Pexp_constant (Pconst_string (attr_value, _)); pexp_loc = a_loc; _ } ->
      { a_loc; a_name; a_value = Val attr_value }
  | e ->
      { a_loc = e.pexp_loc; a_name; a_value = Antiquot e}

and extract_jsx_attr = function
  (* Ignore last unit argument as tyxml api is pure *)
  | Nolabel, [%expr ()] -> None
  | Labelled "children", _ -> None
  | Labelled a_name, a_value -> Some (extract_attr_value a_name a_value)
  | Nolabel, e -> error e.pexp_loc "Unexpected unlabeled jsx attribute"
  | Optional name, e -> error e.pexp_loc "Unexpected optional jsx attribute %s" name

and children_mapper mapper e =
  match e with
  (* Convert string constant into Html.txt "constant" for convenience *)
  | { pexp_desc = Pexp_constant (Pconst_string _); pexp_loc = loc; _ } as str -> [%expr Html.txt [%e str]][@metaloc loc]
  | _ -> expr_mapper mapper e

and expr_mapper mapper e =
  match e with
  (* matches <div foo={bar}> child1 child2 </div>; *)
  | { pexp_attributes = [ ({ txt = "JSX"; loc = _ }, PStr []) ];
    pexp_desc = Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident html_tag; loc = _ }; _ }, args);
      pexp_loc = apply_loc
    } ->
    let attributes = filter_map extract_jsx_attr args |> List.map (fun a -> a.a_name, a.a_value) in
    let args = jsx_args_to_tyxml_args mapper args in
    Element.parse ~loc:apply_loc ~parent_lang:Common.Html ~name:(Common.Html, html_tag) ~attributes args
  | _ -> default_mapper.expr mapper e

let mapper _ _ = { default_mapper with expr = expr_mapper }
