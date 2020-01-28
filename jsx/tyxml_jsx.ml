open Ast_mapper
open Parsetree
open Asttypes

open Tyxml_syntax

let is_jsx e =
  let f = function
    | ({ txt = "JSX"; loc = _ }, _) -> true
    | _ -> false
  in
  List.exists f e.pexp_attributes


let to_kebab_case name =
  let length = String.length name in
  if length > 5 then
    let first = String.sub name 0 4 in
    match first with
    | "aria"
    | "data" ->
      first ^ "-" ^ String.lowercase_ascii (String.sub name 4 (length - 4))
    | _ -> name
  else
    name

let make_html_attr_name name =
  let name =
    match name with
    | "className" -> "class"
    | "htmlFor" -> "for"
    | "class_" -> "class"
    | "for_" -> "for"
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

(** Children *)


let make_txt ~loc ~lang s =
  let txt = Common.make ~loc lang "txt" in
  let arg = Common.wrap lang loc @@ Common.string loc s in
  Ast_helper.Exp.apply ~loc txt [Common.Label.nolabel, arg]

let element_mapper mapper e =
  match e with
  (* Convert string constant into Html.txt "constant" for convenience *)
  | { pexp_desc = Pexp_constant (Pconst_string (str, _)); pexp_loc = loc; _ } ->
    make_txt ~loc ~lang:Html str
  | _ ->
    mapper.expr mapper e

let extract_element_list mapper elements =
  let rec map acc e =
    match e with
    | [%expr []] -> List.rev acc
    | [%expr [%e? child] :: [%e? rest]] ->
      let child = Common.value (element_mapper mapper child) in
      map (child :: acc) rest
    | e ->
      List.rev (Common.antiquot (element_mapper mapper e) :: acc)
  in
  map [] elements

let extract_children mapper args =
  match
    List.find
      (function Labelled "children", _ -> true | _ -> false)
      args
  with
  | _, children -> extract_element_list mapper children
  | exception Not_found -> []

(** Attributes *)

type attr = {
  a_name: Common.name;
  a_value : string value;
  a_loc: Location.t;
}

let rec extract_attr_value a_name a_value =
  let a_name = make_html_attr_name a_name in
  match a_value with
  | { pexp_desc = Pexp_constant (Pconst_string (attr_value, _));
      _;
    } ->
    (a_name, Common.value attr_value)
  | e ->
    (a_name, Common.antiquot e)

and extract_attr = function
  (* Ignore last unit argument as tyxml api is pure *)
  | Nolabel, [%expr ()] -> None
  | Labelled "children", _ -> None
  | Labelled name, value ->
    Some (extract_attr_value name value)
  | Nolabel, e ->
    error e.pexp_loc "Unexpected unlabeled jsx attribute"
  | Optional name, e ->
    error e.pexp_loc "Unexpected optional jsx attribute %s" name


let guess_namespace ~loc lid =
  match lid with
  | Longident.Ldot (Lident "Html", name) -> (Html, name)
  | Ldot (Lident "Svg", name) -> (Svg, name)
  | Lident name -> begin
    match Element.find_assembler (Html, name) with
    | Some ("svg", _) -> Svg, name 
    | Some _ -> Html, name
    | None -> match Element.find_assembler (Svg, name) with
      | Some _ -> Svg, name
      | None -> Common.error loc "Unknown namespace for the element %s" name
  end
  | _ ->
    Common.error loc "Invalid Tyxml tag %a" Pprintast.longident lid

         
let expr_mapper mapper e =
  if not (is_jsx e) then default_mapper.expr mapper e
  else
    let loc = e.pexp_loc in
    match e with
    (* matches <> ... </>; *)
    | [%expr []] 
    | [%expr [%e? _] :: [%e? _]] ->
      let l = extract_element_list mapper e in
      Common.list_wrap_value Common.Html loc l
    (* matches <div foo={bar}> child1 child2 </div>; *)
    | {pexp_desc = Pexp_apply
           ({ pexp_desc = Pexp_ident { txt }; _ }, args )}
      ->
      let namespace, tag = guess_namespace ~loc txt in
      let attributes = filter_map extract_attr args in
      let children = extract_children mapper args in
      Element.parse ~loc
          ~parent_lang:namespace
          ~name:(namespace, tag)
          ~attributes
          children
     | _ -> default_mapper.expr mapper e

let mapper _ _ = { default_mapper with expr = expr_mapper }

let () =
  Driver.register
    ~name:"tyxml-jsx" Versions.ocaml_405
    mapper
