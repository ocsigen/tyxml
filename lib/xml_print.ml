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


module Utf8 = struct
  type utf8 = string
  type encoding = [ `UTF_16 | `UTF_16BE | `UTF_16LE | `UTF_8 | `US_ASCII | `ISO_8859_1]
  let normalize_from ~encoding src =
    let warn = ref false in
    let rec loop d e = match Uutf.decode d with
      | `Uchar _ as u -> ignore (Uutf.encode e u); loop d e
      | `End -> ignore (Uutf.encode e `End)
      | `Malformed _ -> ignore (Uutf.encode e (`Uchar Uutf.u_rep)); warn:=true;loop d e
      | `Await -> assert false
    in
    let d = Uutf.decoder ~encoding (`String src) in
    let buffer = Buffer.create (String.length src) in
    let e = Uutf.encoder `UTF_8 (`Buffer buffer) in
    loop d e;
    Buffer.contents buffer, !warn

  let normalize src = normalize_from ~encoding:`UTF_8 src

  let normalize_html src =
    let warn = ref false in
    let str e s =
      for i = 0 to String.length s - 1 do
        ignore (Uutf.encode e (`Uchar (Char.code s.[i])))
      done in
    let rec loop d e = match Uutf.decode d with
      | `Uchar 34 -> str e "&quot;"; loop d e
      | `Uchar 38 -> str e "&amp;"; loop d e
      | `Uchar 60 -> str e "&lt;"; loop d e
      | `Uchar 62 -> str e "&gt;"; loop d e
      | `Uchar code as u ->
        let u =
          (* Illegal characters in html
             http://en.wikipedia.org/wiki/Character_encodings_in_HTML
             http://www.w3.org/TR/html5/syntax.html *)
          if (* A. control C0 *)
            (code <= 31 && code <> 9 && code <> 10 && code <> 13)
            (* B. DEL + control C1
               - invalid in html
               - discouraged in xml;
                 exept 0x85 see http://www.w3.org/TR/newline
                 but let's discard it anyway *)
            || (code >= 127 && code <= 159)
            (* C. UTF-16 surrogate halves : already discarded by uutf *)
            (* || (code >= 0xD800 && code <= 0xDFFF) *)
            (* D. BOOM related *)
            || code land 0xFFFF = 0xFFFE
            || code land 0xFFFF = 0xFFFF

          then (warn:=true;`Uchar Uutf.u_rep)
          else u in
        ignore (Uutf.encode e u);
        loop d e
      | `End -> ignore (Uutf.encode e `End)
      | `Malformed _ ->
        ignore (Uutf.encode e (`Uchar Uutf.u_rep));
        warn:=true;
        loop d e
      | `Await -> assert false
    in
    let d = Uutf.decoder ~encoding:`UTF_8 (`String src) in
    let buffer = Buffer.create (String.length src) in
    let e = Uutf.encoder `UTF_8 (`Buffer buffer) in
    loop d e;
    Buffer.contents buffer, !warn

end


module Make
    (Xml : Xml_sigs.Iterable)
    (F : sig val emptytags : string list end)
    (O : Xml_sigs.Output) =
struct

  let (++) = O.concat

  open Xml

  let separator_to_string = function
    | Space -> " "
    | Comma -> ", "

  let attrib_value_to_string encode a = match acontent a with
    | AFloat f -> Printf.sprintf "\"%s\"" (string_of_number f)
    | AInt i -> Printf.sprintf "\"%d\"" i
    | AStr s -> Printf.sprintf "\"%s\"" (encode s)
    | AStrL (sep, slist) ->
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
    if F.emptytags = [] || List.mem tag F.emptytags
    then
      (O.put ("<"^tag)
       ++ xh_print_attrs encode attrs
       ++ O.put " />")
    else
      (O.put ("<"^tag)
       ++ xh_print_attrs encode attrs
       ++ O.put ("></"^tag^">"))

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

module Make_typed
    (Xml : Xml_sigs.Iterable)
    (Typed_xml : Xml_sigs.Iterable_typed_xml with module Xml := Xml)
    (O : Xml_sigs.Output) =
struct

  module P = Make(Xml)(Typed_xml.Info)(O)
  let (++) = O.concat

  let print_list ?(encode = encode_unsafe_char) foret =
    O.make (P.xh_print_taglist encode (List.map Typed_xml.toelt foret))

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
       ++ O.put (if advert <> "" then ("<!-- " ^ advert ^ " -->\n") else "\n")
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
