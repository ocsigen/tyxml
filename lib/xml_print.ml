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

let add_unsafe_char b = function
  | '<' -> Buffer.add_string b "&lt;"
  | '>' -> Buffer.add_string b "&gt;"
  | '"' -> Buffer.add_string b "&quot;"
  | '&' -> Buffer.add_string b "&amp;"
  | c when is_control c ->
    Buffer.add_string b "&#" ;
    Buffer.add_string b (string_of_int (Char.code c)) ;
    Buffer.add_string b ";"
  | c -> Buffer.add_char b c

let encode_unsafe_char s =
  let b = Buffer.create (String.length s) in
  String.iter (add_unsafe_char b) s;
  Buffer.contents b

let encode_unsafe_char_and_at s =
  let b = Buffer.create (String.length s) in
  let f = function
    | '@' -> Buffer.add_string b "&#64;"
    | c -> add_unsafe_char b c
  in
  String.iter f s;
  Buffer.contents b

let compose_decl ?(version = "1.0") ?(encoding = "UTF-8") () =
  "<?xml version=\"" ^ version ^ "\" encoding=\"" ^ encoding ^ "\"?>\n"

let compose_doctype dt args =
  "<!DOCTYPE " ^ dt
  ^ (if args = []
     then ""
     else
       " PUBLIC " ^
       String.concat " " (List.map (fun a -> "\"" ^ a ^ "\"") args)) ^ ">"


(* copied form js_of_ocaml: compiler/javascript.ml *)
let string_of_number v =
  if v = infinity
  then "Infinity"
  else if v = neg_infinity
  then "-Infinity"
  else if v <> v
  then "NaN"
  else
    let vint = int_of_float v in
    (* compiler 1000 into 1e3 *)
    if float_of_int vint = v
    then
      let rec div n i =
        if n <> 0 && n mod 10 = 0
        then div (n/10) (succ i)
        else
        if i > 2
        then Printf.sprintf "%de%d" n i
        else string_of_int vint in
      div vint 0
    else
      let s1 = Printf.sprintf "%.12g" v in
      if v = float_of_string s1
      then s1
      else
        let s2 = Printf.sprintf "%.15g" v in
        if v = float_of_string s2
        then s2
        else  Printf.sprintf "%.18g" v

module Make
    (Xml : Xml_sigs.Iterable)
    (F : sig val emptytags : string list end)
    (O : Xml_sigs.Output) =
struct

  let (++) = O.concat

  open Xml


  let quote = O.put "\""
  let equal = O.put "="
  let space = O.put " "
  let comma_space = O.put ", "
  let lt = O.put "<"
  let gt = O.put ">"
  let lt_slash = O.put "</"
  let space_slash_gt = O.put " />"
  let comment_open = O.put "<!--"
  let comment_close = O.put "-->"
  let ampersand = O.put "&"
  let semicolon = O.put ";"

  let separator_to_m = function
    | Space -> space
    | Comma -> comma_space

  let rec xh_print_attr_value_list encode sep = function
    | [] -> O.empty
    | [x] -> O.put (encode x)
    | x::xs -> O.put (encode x) ++ sep ++ xh_print_attr_value_list encode sep xs

  let xh_print_attr_value encode a =
    quote
    ++ begin match acontent a with
      | AFloat f -> O.put (string_of_number f)
      | AInt i -> O.put (string_of_int i)
      | AStr s -> O.put (encode s)
      | AStrL (sep, slist) ->
        xh_print_attr_value_list encode (separator_to_m sep) slist
    end
    ++ quote


  let xh_print_attr encode a =
    O.put (aname a)
    ++ equal
    ++ xh_print_attr_value encode a

  let rec xh_print_attrs encode attrs = match attrs with
    | [] -> O.empty
    | attr::queue ->
      space
      ++ xh_print_attr encode attr
      ++ xh_print_attrs encode queue

  and xh_print_text texte = O.put texte

  and xh_print_closedtag encode tag attrs =
    if F.emptytags = [] || List.mem tag F.emptytags
    then begin
      lt
      ++ O.put tag
      ++ xh_print_attrs encode attrs
      ++ space_slash_gt
    end
    else begin
      lt
      ++ O.put tag
      ++ xh_print_attrs encode attrs
      ++ gt
      ++ lt_slash
      ++ O.put tag
      ++ gt
    end

  and xh_print_tag encode tag attrs taglist =
    if taglist = []
    then xh_print_closedtag encode tag attrs
    else begin
      lt
      ++ O.put tag
      ++ xh_print_attrs encode attrs
      ++ gt
      ++ xh_print_taglist encode taglist
      ++ lt_slash
      ++ O.put tag
      ++ gt
    end

  and print_nodes encode name xh_attrs xh_taglist queue =
    xh_print_tag encode name xh_attrs xh_taglist
    ++ xh_print_taglist encode queue

  and xh_print_taglist encode taglist =
    match taglist with

    | [] -> O.empty

    | elt :: queue -> match content elt with

      | Comment texte ->
        comment_open
        ++ O.put (encode texte)
        ++ comment_close
        ++ xh_print_taglist encode queue

      | Entity e ->
        ampersand
        ++ O.put (encode e)
        ++ semicolon
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

module Make_typed
    (Xml : Xml_sigs.Iterable)
    (Typed_xml : Xml_sigs.Iterable_typed_xml with module Xml := Xml)
    (O : Xml_sigs.Output) =
struct

  module P = Make(Xml)(Typed_xml.Info)(O)
  let (++) = O.concat

  let print_list ?(encode = encode_unsafe_char) foret =
    O.make (P.xh_print_taglist encode (List.map Typed_xml.toelt foret))

  let advert_open = O.put "<!-- "
  let advert_close = O.put " -->"
  let new_line = O.put "\n"

  let print ?(encode = encode_unsafe_char) ?(advert = "") doc =
    let doc = Typed_xml.doc_toelt doc in
    let doc = match Xml.content doc with
      | Xml.Node (n, a, c) ->
        let a =
          if List.exists (fun a -> Xml.aname a = "xmlns") a
          then a
          else Xml.string_attrib "xmlns" Typed_xml.Info.namespace :: a
        in
        Xml.node ~a n c
      | _ -> doc in
    O.make
      (O.put Typed_xml.Info.doctype
       ++ (if advert <> ""
           then begin
             advert_open
             ++ O.put advert
             ++ advert_close
           end else
             O.empty)
       ++ new_line
       ++ P.xh_print_taglist encode [doc])

end

module Simple_output(M : sig val put: string -> unit end) = struct
  type out = unit
  type m = unit -> unit
  let empty () = ()
  let concat f1 f2 () = f1 (); f2 ()
  let put s () = M.put s
  let make f = f ()
end

module Make_simple
    (Xml : Xml_sigs.Iterable)
    (I : sig val emptytags : string list end) =
struct

  type elt = Xml.elt
  type out = unit
  let print_list ~output =
    let module M = Make(Xml)(I)(Simple_output(struct let put = output end)) in
    M.print_list

end

module Make_typed_simple
    (Xml : Xml_sigs.Iterable)
    (Typed_xml : Xml_sigs.Typed_xml with  module Xml := Xml) =
struct

  type out = unit
  type 'a elt = 'a Typed_xml.elt
  type doc = Typed_xml.doc

  let print_list ~output =
    let module M =
      Make_typed(Xml)(Typed_xml)(Simple_output(struct let put = output end)) in
    M.print_list

  let print ~output =
    let module M =
      Make_typed(Xml)(Typed_xml)(Simple_output(struct let put = output end)) in
    M.print

end
