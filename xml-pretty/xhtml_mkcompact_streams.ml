(* Ocsigen
 * Copyright (C) 2008 Vincent Balat, Mauricio Fernandez
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Format
open XML
module MakeCompact (F : Xhtml_format.Info)(S : Xhtml_streams.STREAM) = struct

(*****************************************************************************)
(* print to streams *)

  let (>>) m f = S.bind m (fun () -> f)
  let (<<) f m = S.bind m (fun () -> f)

  let aux ~width ~encode ?(html_compat = false) arbre =
    let endemptytag = if html_compat then ">" else " />" in
    let rec xh_print_attrs encode attrs = match attrs with
    | [] -> S.return ()
    | attr::queue ->
        S.put (" "^XML.attrib_to_string encode attr)
	  >> xh_print_attrs encode queue

    and xh_print_text texte = S.put texte

    and xh_print_closedtag encode tag attrs =
      if List.mem tag F.emptytags
      then
        (S.put ("<"^tag)
	   >> xh_print_attrs encode attrs
	   >> S.put endemptytag)
      else
        (S.put ("<"^tag)
           >> xh_print_attrs encode attrs
           >> S.put ("></"^tag^">"))

    and xh_print_tag encode tag attrs taglist =
      if taglist = []
      then xh_print_closedtag encode tag attrs
      else
        (S.put ("<"^tag)
           >> xh_print_attrs encode attrs
           >> S.put ">"
           >> xh_print_taglist taglist
           >> S.put ("</"^tag^">"))

    and print_nodes name xh_attrs xh_taglist queue =
      xh_print_tag encode name xh_attrs xh_taglist
        >> xh_print_taglist queue

    and xh_print_taglist taglist =
      match taglist with

      | [] -> S.return ()

      | { elt = Comment texte }::queue ->
          xh_print_text ("<!--"^(encode texte)^"-->")
            >> xh_print_taglist queue

      | { elt = Entity e }::queue ->
          xh_print_text ("&"^e^";") (* no encoding *)
            >> xh_print_taglist queue

      | { elt = PCDATA texte }::queue ->
          xh_print_text (encode texte)
            >> xh_print_taglist queue

      | { elt = EncodedPCDATA texte }::queue ->
          xh_print_text texte
            >> xh_print_taglist queue

      (* Nodes and Leafs *)
      | { elt = Node (name, xh_attrs, xh_taglist )}::queue ->
          print_nodes name xh_attrs xh_taglist queue

      | { elt = Leaf (name,xh_attrs )}::queue ->
          print_nodes name xh_attrs [] queue

      | { elt = Empty }::queue ->
          xh_print_taglist queue

      in
      xh_print_taglist [arbre]

  let opt_default x = function
    | Some x -> x
    | _ -> x


  let xhtml_stream ?version ?(width=132) ?(encode = encode_unsafe) ?html_compat arbre =
    let version = opt_default F.default_doctype version in
    S.put (F.doctype version)
      >> S.put F.ocsigenadv
      >> aux ?width ?encode ?html_compat (F.toelt arbre)

  let xhtml_list_stream
      ?(width=132) ?(encode = encode_unsafe) ?html_compat foret =
    List.fold_right (<<)
      (List.map (fun arbre -> aux ?width ?encode ?html_compat (F.toelt arbre)) foret)
      (S.return ())

end
