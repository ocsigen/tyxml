(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2018 Gabriel Radanne
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
 * Foundation, Inc., 51 Franklin Street, Suite 500, Boston, MA 02110-1301, USA.
*)

type name = string * string

(** Input *)

type signal = [
  | `Comment of string
  | `End_element
  | `Start_element of name * (name * string) list
  | `Text of string list
]

exception Malformed_stream

module Import
    (Xml : Xml_sigs.T)
= struct

  let of_list l =
    List.fold_right
      (fun a b -> Xml.W.(cons (return a) b))
      l (Xml.W.nil ())

  let mk_attribs attrs =
    (* TODO: This is not very structured *)
    let f ((_,name), v) = Xml.string_attrib name (Xml.W.return v) in
    List.map f attrs

  let rec mk children (seq : signal Seq.t) = match seq () with
    | Cons (`Comment s, q) ->
      mk (Xml.comment s :: children) q
    | Cons (`Text s, q) ->
      mk (List.map (fun x -> Xml.pcdata @@ Xml.W.return x) s @ children) q
    | Cons (`Start_element ((_, name), attrs), q) ->
      let a = mk_attribs attrs in
      let sub_children, rest = mk [] q in
      mk (Xml.node ~a name sub_children :: children) rest
    | Cons (`End_element, rest) ->
      of_list (List.rev children), rest
    | Nil ->
      of_list (List.rev children), Seq.empty

  let of_seq seq =
    let l, rest = mk [] seq in
    match rest () with
    | Seq.Nil -> l
    | _ -> raise Malformed_stream

end

(** Output *)

type output = [ signal | `Raw of string list ]

module Export
    (Xml : Xml_sigs.Iterable)
= struct

  let mk ~ns name = (ns, name)

  let convert_attributes ~ns attributes =
    attributes |> List.map @@ fun attribute ->
    let value =
      match Xml.acontent attribute with
      | AFloat n -> Xml_print.string_of_number n
      | AInt n -> string_of_int n
      | AStr s -> s
      | AStrL (Space, ss) -> String.concat " " ss
      | AStrL (Comma, ss) -> String.concat ", " ss
    in
    (mk ~ns (Xml.aname attribute), value)

  let (++) x l = Seq.Cons (x, l)
  let rec mk_elt ~ns x q () : output Seq.node =
    match Xml.content x with
    | Empty -> q ()
    | Comment s -> `Comment s ++ q
    | EncodedPCDATA s ->  `Raw [s] ++ q
    | PCDATA s -> `Text [s] ++ q
    | Entity s -> `Raw ["&"^s^";"] ++ q
    | Leaf (name, attributes) ->
      `Start_element (mk ~ns name, convert_attributes ~ns attributes) ++
      fun () -> `End_element ++ q
    | Node (name, attributes, children) ->
      `Start_element (mk ~ns name, convert_attributes ~ns attributes) ++
      mk_list ~ns children q
  and mk_list ~ns l q () : output Seq.node =
    match l with
    | [] -> Seq.Nil
    | h :: t -> mk_elt ~ns h (mk_list ~ns t q) ()

  let to_seq ?(namespace="") xml : output Seq.t =
    mk_elt ~ns:namespace xml Seq.empty
  let to_seql ?(namespace="") l : output Seq.t =
    mk_list ~ns:namespace l Seq.empty
end

module Typed_export
    (Xml : Xml_sigs.Iterable)
    (Typed_xml : Xml_sigs.Typed_xml with module Xml := Xml)
= struct
  module E = Export(Xml)
  let export l =
    E.to_seql ~namespace:Typed_xml.Info.namespace @@ Typed_xml.toeltl l
end
