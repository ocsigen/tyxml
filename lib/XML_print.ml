(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2008 Vincent Balat, Mauricio Fernandez
 * Copyright (C) 2011 Pierre Chambart, Grégoire Henry
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

let is_control c =
  let cc = Char.code c in
  (cc <= 8 || cc = 11 || cc = 12 || (14 <= cc && cc <= 31) || cc = 127)

let encode_unsafe_char s =
  let b = Buffer.create (String.length s) in
  String.iter (function
    | '<' -> Buffer.add_string b "&lt;"
    | '>' -> Buffer.add_string b "&gt;"
    | '"' -> Buffer.add_string b "&quot;"
    | '&' -> Buffer.add_string b "&amp;"
    | c when is_control c ->
        Buffer.add_string b ("&#" ^ string_of_int (Char.code c) ^ ";")
    | c -> Buffer.add_char b c) s;
  Buffer.contents b

let encode_unsafe_char_and_at s =
  let b = Buffer.create (String.length s) in
  String.iter (function
    | '<' -> Buffer.add_string b "&lt;"
    | '>' -> Buffer.add_string b "&gt;"
    | '"' -> Buffer.add_string b "&quot;"
    | '&' -> Buffer.add_string b "&amp;"
    | '@' -> Buffer.add_string b "&#64;"
    | c when is_control c ->
        Buffer.add_string b ("&#" ^ string_of_int (Char.code c) ^ ";")
    | c -> Buffer.add_char b c) s;
  Buffer.contents b

let compose_decl ?(version = "1.0") ?(encoding = "UTF-8") () =
  "<?xml version=\"" ^ version ^ "\" encoding=\"" ^ encoding ^ "\"?>\n"

let compose_doctype dt args =
  "<!DOCTYPE " ^ dt ^ " PUBLIC " ^
  String.concat " " (List.map (fun a -> "\"" ^ a ^ "\"") args) ^ ">\n"

module Make(XML : XML_sigs.Iterable)(F : sig val emptytags : string list end)(O : XML_sigs.Output) = struct

  let (++) = O.concat

  open XML

  let separator_to_string = function
    | Space -> " "
    | Comma -> ", "

  let attrib_value_to_string encode a = match acontent a with
  | AFloat (_, f) -> Printf.sprintf "\"%e\"" f
  | AInt (_, i) -> Printf.sprintf "\"%d\"" i
  | AStr (_, s) -> Printf.sprintf "\"%s\"" (encode s)
  | AStrL (sep, _, slist) ->
      Printf.sprintf "\"%s\""
        (encode (String.concat (separator_to_string sep) slist))

  let attrib_to_string encode a =
    Printf.sprintf "%s=%s" (aname a) (attrib_value_to_string encode a)

  let rec xh_print_attrs encode attrs = match attrs with
  | [] -> O.empty
  | attr::queue ->
      O.put (" "^ attrib_to_string encode attr)
	++ xh_print_attrs encode queue

  and xh_print_text texte = O.put texte

  and xh_print_closedtag encode tag attrs =
    if List.mem tag F.emptytags
    then
      (O.put ("<"^tag)
	 ++ xh_print_attrs encode attrs
	 ++ O.put ">")
    else
      (O.put ("<"^tag)
         ++ xh_print_attrs encode attrs
         ++ O.put ((if F.emptytags = [] then " />" else ("></"^tag^">"))))

  and xh_print_tag encode tag attrs taglist =
    if taglist = []
    then xh_print_closedtag encode tag attrs
    else
      (O.put ("<"^tag)
         ++ xh_print_attrs encode attrs
         ++ O.put ">"
         ++ xh_print_taglist encode taglist
         ++ O.put ("</"^tag^">"))

  and print_nodes encode name xh_attrs xh_taglist queue =
    xh_print_tag encode name xh_attrs xh_taglist
      ++ xh_print_taglist encode queue

  and xh_print_taglist encode taglist =
    match taglist with

    | [] -> O.empty

    | elt :: queue -> match content elt with

    | Comment texte ->
        O.put ("<!--"^(encode texte)^"-->")
	  ++ xh_print_taglist encode queue

    | Entity e ->
        O.put ("&"^e^";") (* no encoding *)
          ++ xh_print_taglist encode queue

    | PCDATA texte ->
        O.put (encode texte)
          ++ xh_print_taglist encode queue

    | EncodedPCDATA texte ->
        O.put texte
          ++ xh_print_taglist encode queue

    | Node (name, xh_attrs, xh_taglist) ->
        print_nodes encode name xh_attrs xh_taglist queue

    | Leaf (name, xh_attrs) ->
        print_nodes encode name xh_attrs [] queue

    | Empty ->
        xh_print_taglist encode queue

  let print_list ?(encode = encode_unsafe_char) foret =
    O.make (xh_print_taglist encode foret)

end

module MakeTyped(XML : XML_sigs.Iterable)
                (TypedXML : XML_sigs.IterableTypedXML with module XML := XML)
                (O : XML_sigs.Output) = struct

  module P = Make(XML)(TypedXML.Info)(O)
  let (++) = O.concat

  let print_list ?(encode = encode_unsafe_char) foret =
    O.make (P.xh_print_taglist encode (List.map TypedXML.toelt foret))

  let print ?(encode = encode_unsafe_char) ?(advert = "") doc =
    O.make
      (O.put TypedXML.Info.doctype
	 ++ O.put (if advert <> "" then ("<!-- " ^ advert ^ " -->\n") else "\n")
	 ++ P.xh_print_taglist encode [TypedXML.doc_toelt doc])

end

module SimpleOutput(M : sig val put: string -> unit end) = struct
  type out = unit
  type m = unit -> unit
  let empty () = ()
  let concat f1 f2 () = f1 (); f2 ()
  let put s () = M.put s
  let make f = f ()
end

module MakeSimple(XML : XML_sigs.Iterable)(I : sig val emptytags : string list end) = struct

  type elt = XML.elt
  type out = unit
  let print_list ~output =
    let module M = Make(XML)(I)(SimpleOutput(struct let put = output end)) in
    M.print_list

end

module MakeTypedSimple(XML : XML_sigs.Iterable)
                      (TypedXML : XML_sigs.TypedXML with  module XML := XML) = struct

  type out = unit
  type 'a elt = 'a TypedXML.elt
  type doc = TypedXML.doc

  let print_list ~output =
    let module M = MakeTyped(XML)(TypedXML)(SimpleOutput(struct let put = output end)) in
    M.print_list

  let print ~output =
    let module M = MakeTyped(XML)(TypedXML)(SimpleOutput(struct let put = output end)) in
    M.print

end
