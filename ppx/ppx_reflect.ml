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

(* Runs on [html5_sigs.mli], [svg_sigs.mli], and [html5_types.mli]. Certain type
   and value declarations are read for type information, which is stored in
   corresponding [_reflected] files - for example, [html5_sigs.mli] results in
   [html5_sigs_reflected.ml]. See comments by functions below and in
   [ppx_sigs_reflected.mli] for details. *)

open Ast_mapper
open Asttypes
open Parsetree



let is_attribute s = String.length s >= 2 && String.sub s 0 2 = "a_"

let strip_a s =
  if String.length s < 2 || String.sub s 0 2 <> "a_" then s
  else String.sub s 2 (String.length s - 2)

let argument_types t =
  let rec scan acc = function
    | Ptyp_arrow (_, t, t') -> scan (t::acc) t'.ptyp_desc
    | _ -> List.rev acc
  in
  scan [] t.ptyp_desc



(* Given the name of a TyXML attribute function and a list of its argument
   types, selects the attribute value parser (in module [Ppx_attribute_value])
   that should be used for that attribute. *)
let type_to_attribute_parser name types =
  let rec no_constructor_arguments = function
    | [] -> true
    | (Rinherit _)::_
    | (Rtag (_, _, _, _::_))::_ -> false
    | (Rtag (_, _, _, []))::more -> no_constructor_arguments more
  in

  match types with
  | [] ->
    "nowrap presence"

  | [[%type : character wrap]] ->
    "wrap char"

  | [[%type : bool wrap]] ->
    "wrap bool"

  | [[%type : number wrap]]
  | [[%type : pixels wrap]]
  | [[%type : int wrap]] ->
    "wrap int"

  | [[%type : numbers wrap]] ->
    "wrap (commas int)"

  | [[%type : float_number wrap]]
  | [[%type : float wrap]] ->
    "wrap float"

  | [[%type : float_number option wrap]] ->
    "wrap (option \"any\" float)"

  | [[%type : numbers_semicolon wrap]] ->
    "wrap (semicolons float)"

  | [[%type : fourfloats wrap]] ->
    "wrap fourfloats"

  | [[%type : number_optional_number wrap]] ->
    "wrap number_pair"

  | [[%type : coords wrap]] ->
    "wrap points"

  | [[%type : (number * number) list option wrap]] ->
    "wrap (option \"any\" (spaces icon_size))"

  | [[%type : length wrap]] ->
    "wrap length"

  | [[%type : multilengths wrap]] ->
    "wrap (commas multilength)"

  | [[%type : coord wrap]]
  | [[%type : Unit.length wrap]] ->
    "wrap svg_length"

  | [[%type : Unit.length list wrap]] ->
    "wrap (spaces_or_commas svg_length)"

  | [[%type : Unit.angle option wrap]] ->
    "wrap (option \"auto\" angle)"

  | [[%type : string wrap]]
  | [[%type : text wrap]]
  | [[%type : nmtoken wrap]]
  | [[%type : idref wrap]]
  | [[%type : Xml.uri wrap]]
  | [[%type : contenttype wrap]]
  | [[%type : languagecode wrap]]
  | [[%type : cdata wrap]]
  | [[%type : charset wrap]]
  | [[%type : frametarget wrap]]
  | [[%type : iri wrap]]
  | [[%type : color wrap]]
  | [[%type : nmtoken]; [%type : text wrap]] ->
    "wrap string"

  | [[%type : Xml.event_handler]]
  | [[%type : Xml.mouse_event_handler]]
  | [[%type : Xml.keyboard_event_handler]] ->
    "nowrap string"

  | [[%type : string option wrap]] ->
    "wrap (option \"\" string)"

  | [[%type :
      [%t? {ptyp_desc = Ptyp_variant (_::_::_ as constructors, _, _)}] wrap]]
      when no_constructor_arguments constructors ->
    "wrap variant"

  | [[%type : shape wrap]] ->
    "wrap variant"

  | [[%type : nmtokens wrap]]
  | [[%type : idrefs wrap]]
  | [[%type : charsets wrap]]
  | [[%type : spacestrings wrap]]
  | [[%type : strings wrap]] ->
    "wrap (spaces string)"

  | [[%type : commastrings wrap]]
  | [[%type : text list wrap]]
  | [[%type : contenttypes wrap]] ->
    "wrap (commas string)"

  | [[%type : linktypes wrap]] ->
    "wrap (spaces (total_variant Html5_types_reflected.linktype))"

  | [[%type : mediadesc wrap]] ->
    "wrap (commas (total_variant Html5_types_reflected.mediadesc_token))"

  | [[%type : transform wrap]] ->
    "wrap transform"

  | [[%type : lengths wrap]] ->
    "wrap (spaces_or_commas svg_length)"

  | [[%type : transforms wrap]] ->
    "wrap (spaces_or_commas transform)"

  | [[%type : paint wrap]] ->
    "wrap paint"

  | [[%type : image_candidate list wrap]] ->
    "wrap (commas srcset_element)"

  | _ ->
    let name = strip_a name in
    let name = if name = "in" then "in_" else name in
    Printf.sprintf "wrap %s" name

(* Given a list of attributes from a val declaration whose name begins with a_,
   checks if the declaration has a [@@reflect.attribute] annotation. If so, the
   declaration's name does not directly correspond to markup attribute name
   (e.g. "a_input_max" does not directly correspond to "max"). The annotation is
   parsed to get the markup name and the element types in which the translation
   from markup name to TyXML name should be performed. *)
let ocaml_attributes_to_renamed_attribute name attributes =
  let maybe_attribute =
    attributes
    |> Ppx_common.find (fun attr -> (fst attr).txt = "reflect.attribute")
  in

  match maybe_attribute with
  | None -> []
  | Some ({loc}, payload) ->
    match payload with
    | PStr [%str
        [%e? {pexp_desc = Pexp_constant (Const_string (real_name, _))}]
        [%e? element_names]] ->
      let element_names =
        let rec traverse acc = function
          | [%expr
              [%e? {pexp_desc =
                Pexp_constant (Const_string (element_name, _))}]::
                  [%e? tail]] ->
            traverse (element_name::acc) tail
          | [%expr []] -> acc
          | {pexp_loc} ->
            Ppx_common.error pexp_loc
              "List in [@@reflect.attribute] must contain strings"
        in
        traverse [] element_names
      in

      [name, real_name, element_names]

    | _ ->
      Ppx_common.error loc
        "Payload of [@@reflect.attribute] must be a string and a string list"

(* Given a val declaration, determines whether it is for an element. If so,
   evaluates to the element's child assembler (from module
   [Ppx_element_content]), list of attributes passed as labeled arguments, and
   markup name, if different from its TyXML name (for example, [object_] is
   [object] in markup).

   A val declaration is for an element if it either has a [@@reflect.element]
   attribute, or its result type is [_ nullary], [_ unary], or [_ star]. *)
let val_item_to_element_info value_description =
  let name = value_description.pval_name.txt in

  let maybe_attribute =
    value_description.pval_attributes
    |> Ppx_common.find (fun attr -> (fst attr).txt = "reflect.element")
  in

  let maybe_assembler, real_name =
    match maybe_attribute with
    | Some ({loc}, payload) ->
      begin match payload with
      | PStr [%str
          [%e? {pexp_desc = Pexp_constant (Const_string (assembler, _))}]] ->
        Some assembler, None

      | PStr [%str
          [%e? {pexp_desc = Pexp_constant (Const_string (assembler, _))}]
          [%e? {pexp_desc = Pexp_constant (Const_string (name, _))}]] ->
        Some assembler, Some name

      | _ ->
        Ppx_common.error loc
          "Payload of [@@reflect.element] must be a one or two strings"
      end

    | None ->
      let result_type =
        let rec scan = function
          | {ptyp_desc = Ptyp_arrow (_, _, t')} -> scan t'
          | t -> t
        in
        scan value_description.pval_type
      in

      match result_type with
      | [%type : ([%t? _], [%t ? _]) nullary] -> Some "nullary", None
      | [%type : ([%t? _], [%t ? _], [%t ? _]) unary] -> Some "unary", None
      | [%type : ([%t? _], [%t ? _], [%t ? _]) star] -> Some "star", None
      | _ -> None, None
  in

  match maybe_assembler with
  | None -> None
  | Some assembler ->
    let labeled_attributes =
      let rec scan acc = function
        | Ptyp_arrow (label, t, t') ->
          let label =
            if label = "" || label.[0] <> '?' then label
            else String.sub label 1 (String.length label - 1)
          in
          if label = "" then scan acc t'.ptyp_desc
          else begin
            let maybe_attribute_type =
              match t with
              | [%type : [%t? _] wrap] ->
                Some t

              | {ptyp_desc = Ptyp_constr (lid, [[%type : [%t? _] elt wrap]])}
                  when Longident.last lid.txt = "option" ->
                None

              | {ptyp_desc =
                  Ptyp_constr (lid, [[%type : [%t? _] wrap] as t''])}
                  when Longident.last lid.txt = "option" ->
                Some t''

              | _ ->
                None
            in

            match maybe_attribute_type with
            | None -> scan acc t'.ptyp_desc
            | Some t'' ->
              let parser = type_to_attribute_parser label [t''] in
              scan ((name, label, parser)::acc) t'.ptyp_desc
          end

        | _ -> acc
      in
      scan [] value_description.pval_type.ptyp_desc
    in

    let rename =
      match real_name with
      | None -> []
      | Some real_name -> [real_name, name]
    in

    Some (assembler, labeled_attributes, rename)



let attribute_parsers = ref []
let labeled_attributes = ref []
let renamed_attributes = ref []
let element_assemblers = ref []
let renamed_elements = ref []

(* Walks over signature items, looking for elements and attributes. Calls the
   functions immediately above, and accumulates their results in the above
   references. This function is relevant for [html5_sigs.mli] and
   [svg_sigs.mli]. *)
let signature_item mapper item =
  begin match item.psig_desc with
  | Psig_value {pval_name = {txt = name}; pval_type = type_; pval_attributes}
      when is_attribute name ->
    (* Attribute declaration. *)

    let argument_types = argument_types type_ in
    let attribute_parser_mapping =
      name, type_to_attribute_parser name argument_types in
    attribute_parsers := attribute_parser_mapping::!attribute_parsers;

    let renaming = ocaml_attributes_to_renamed_attribute name pval_attributes in
    renamed_attributes := renaming @ !renamed_attributes

  | Psig_value v ->
    (* Non-attribute, but potentially an element declaration. *)

    begin match val_item_to_element_info v with
    | None -> ()
    | Some (assembler, labeled_attributes', rename) ->
      element_assemblers := (v.pval_name.txt, assembler)::!element_assemblers;
      labeled_attributes := labeled_attributes' @ !labeled_attributes;
      renamed_elements := rename @ !renamed_elements
    end

  | _ -> ()
  end;

  default_mapper.signature_item mapper item



let reflected_variants = ref []

(* Walks over type declarations (which will be in signature items). For each
   that is marked with [@@reflect.total_variant], expects it to be a polymorphic
   variant. Splits the constructors into those that have no arguments, and one
   constructor that has one string argument. This constructor information is
   accumulated in [reflected_variants]. This function is relevant for
   [html5_types.mli]. *)
let type_declaration mapper declaration =
  let is_reflect attr = (fst attr).txt = "reflect.total_variant" in
  if List.exists is_reflect declaration.ptype_attributes then begin
    let name = declaration.ptype_name.txt in

    match declaration.ptype_manifest with
    | Some {ptyp_desc = Ptyp_variant (rows, _, _); ptyp_loc} ->
      let rows =
        rows |> List.map (function
          | Rtag (label, _, _, types) -> label, types
          | Rinherit {ptyp_loc} ->
            Ppx_common.error ptyp_loc
              "Inclusion is not supported by [@@refect.total_variant]")
      in

      let nullary, unary =
        rows |> List.partition (fun (_, types) -> types = []) in

      let unary =
        match unary with
        | [name, [[%type : string]]] -> name
        | _ ->
          Ppx_common.error ptyp_loc
            "Expected exactly one non-nullary constructor `C of string"
      in

      let nullary = nullary |> List.map fst in

      reflected_variants := (name, (unary, nullary))::!reflected_variants

    | _ ->
      Ppx_common.error declaration.ptype_loc
        "[@@reflect.total_variant] expects a polymorphic variant type"
  end;

  default_mapper.type_declaration mapper declaration



(* Creates an AST mapper that applies [signature_item] and [type_declaration],
   then formats the generated reflection information as ML code to the file
   whose name is given in the first argument to the PPX reflector. *)
let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s FILE\n" Sys.argv.(0);
    exit 2
  end;

  let filename = Sys.argv.(1) in

  register "reflect_sig" (fun _ ->
    {default_mapper with signature_item; type_declaration});

  (* The channel will be closed on process exit. *)
  let channel = open_out filename in
  let write f = Printf.fprintf channel f in

  if !attribute_parsers <> [] then begin
    write "open Ppx_attribute_value\n";

    write "\nlet attribute_parsers = [\n";
    !attribute_parsers |> List.iter (fun (name, parser) ->
      write "  %S, %s;\n" name parser);
    write "]\n";

    write "\nlet renamed_attributes = [\n";
    !renamed_attributes |> List.iter (fun (name, real_name, element_names) ->
      write "  %S, %S, [" name real_name;
      element_names
      |> List.map (Printf.sprintf "%S")
      |> String.concat "; "
      |> write "%s];\n");
    write "]\n";

    write "\nlet labeled_attributes = [\n";
    !labeled_attributes |> List.iter (fun (name, label, parser) ->
      write "  %S, %S, %s;\n" name label parser);
    write "]\n";

    write "\nopen Ppx_element_content\n";

    write "\nlet element_assemblers = [\n";
    !element_assemblers |> List.iter (fun (name, assembler) ->
      write "  %S, %s;\n" name assembler);
    write "]\n";

    write "\nlet renamed_elements = [\n";
    !renamed_elements |> List.iter (fun (real_name, name) ->
      write "  %S, %S;\n" real_name name);
    write "]\n"
  end;

  !reflected_variants |> List.iter (fun (name, (unary, nullary)) ->
    write "\nlet %s = %S, [\n" name unary;
    nullary |> List.iter (fun nullary ->
      write "  %S;\n" nullary);
    write "]\n")
