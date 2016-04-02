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

let parse loc (ns, element_name) attributes =
  let language, (module Reflected) =
    Ppx_namespace.reflect loc ns in

  (* For attribute names ["data-foo"], evaluates to [Some "foo"], otherwise
     evaluates to [None]. *)
  let parse_user_data local_name =
    let prefix = "data-" in
    let length = String.length prefix in

    let is_user_data =
      try language = Html && String.sub local_name 0 length = prefix
      with Invalid_argument _ -> false
    in

    if not is_user_data then None
    else Some (String.sub local_name length (String.length local_name - length))
  in

  (* Applied to each attribute. Accumulates individually labeled attributes,
     such as img/src, in "labeled," and attributes passed in ~a in "regular." *)
  let parse_attribute (labeled, regular) ((_, local_name), value) =
    (* Convert the markup name of the attribute to a TyXML name without regard
       to renamed attributes such as "a_input_max." Renaming will be accounted
       for later. *)
    let tyxml_name = Tyxml_name.attrib local_name in

    let test_labeled (e, a, _) = e = element_name && a = local_name in
    let test_blacklisted (a, _, _) = a = tyxml_name in
    let test_renamed (_, a, es) = a = local_name && List.mem element_name es in

    let unknown () =
      Ppx_common.error loc "Unknown attribute in %s element: %s"
        (Ppx_common.lang language) local_name
    in

    (* Check whether this attribute is individually labeled. Parse its argument
       and accumulate the attribute if so. *)
    match Ppx_common.find test_labeled Reflected.labeled_attributes with
    | Some (_, label, parser) ->
      let e =
        match parser language loc local_name value with
        | None ->
          Ppx_common.error loc
            "Internal error: labeled attribute %s without an argument" label
        | Some e -> e
      in

      (Ppx_common.Label.labelled label, e)::labeled, regular

    | None ->
      (* The attribute is not individually labeled, so it is passed in ~a.

         First, check if the default TyXML name of this attribute collides with
         the TyXML name of a renamed attribute. For example, if the language is
         HTML, and this attribute has markup name "input-max" (which is
         invalid), then its default TyXML name will be "a_input_max", which is a
         *valid* value in TyXML. We want to avoid mapping "input-max" to
         "a_input_max", because "input-max" is invalid, and because
         "a_input_max" maps to "max" instead. *)
      if List.exists test_blacklisted Reflected.renamed_attributes then
        unknown ()
      else
        (* Check if this is a "data-foo" attribute. Parse the attribute value,
           and accumulate it in the list of attributes passed in ~a. *)
        match parse_user_data local_name with
        | Some tag ->
          let tyxml_name = "a_user_data" in

          let parser =
            try List.assoc tyxml_name Reflected.attribute_parsers
            with Not_found ->
              Ppx_common.error loc "Internal error: no parser for %s" tyxml_name
          in

          let identifier = Ppx_common.make ~loc language tyxml_name in
          let tag = Ppx_common.string loc tag in

          let e =
            match parser language loc local_name value with
            | Some e' -> [%expr [%e identifier] [%e tag] [%e e']] [@metaloc loc]
            | None ->
              Ppx_common.error loc "Internal error: no expression for %s"
                tyxml_name
          in

          labeled, e::regular

        | None ->
          let tyxml_name =
            match Ppx_common.find test_renamed Reflected.renamed_attributes with
            | Some (name, _, _) -> name
            | None -> tyxml_name
          in

          let parser =
            try List.assoc tyxml_name Reflected.attribute_parsers
            with Not_found -> unknown ()
          in

          let identifier = Ppx_common.make ~loc language tyxml_name in

          let e =
            match parser language loc local_name value with
            | None -> identifier
            | Some e' -> [%expr [%e identifier] [%e e']] [@metaloc loc]
          in

          labeled, e::regular
  in

  let labeled, regular =
    List.fold_left parse_attribute ([], []) attributes in

  (* If there are any attributes to pass in ~a, assemble them into a parse tree
     for a list, and prefix that with the ~a label. *)
  if regular = [] then List.rev labeled
  else
    let regular =
      Ppx_common.Label.labelled "a",
      Ppx_common.list loc (List.rev regular)
    in
    List.rev (regular::labeled)
