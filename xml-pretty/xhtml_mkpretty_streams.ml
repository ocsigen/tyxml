(* Ocsigen
 * Copyright (C) 2007 Vincent Balat
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
open Xhtml_format

module MakePretty (F : Info)(S : Xhtml_streams.Stream) = struct
  let id x = x
  let taille_tab = 2

  type s = S.s
  include F

  let (++) = S.concat
  let (<<) f m = S.concat m f


(*****************************************************************************)
(* print to streams *)

  let aux ~width ~encode ?(html_compat = false) arbre =

    let endemptytag = if html_compat then ">" else " />" in
    let rec xh_print_attrs encode attrs = match attrs with
    | [] -> S.empty ()
    | attr::queue ->
        S.put (" "^XML.attrib_to_string encode attr)
	  ++ xh_print_attrs encode queue

    and xh_print_text texte i is_first = S.put texte

    and xh_print_closedtag encode tag attrs i is_first =
      if List.mem tag F.emptytags
      then
	(S.put (if (i > 0) || is_first then String.make (taille_tab*i) ' ' else "")
           ++ S.put ("<"^tag)
           ++ xh_print_attrs encode attrs
	   ++ S.put endemptytag)
      else
	(S.put (if (i > 0) || is_first then String.make (taille_tab*i) ' ' else "")
	   ++ S.put ("<"^tag)
	   ++ xh_print_attrs encode attrs
	   ++ S.put ("></"^tag^">"))

    and xh_print_inlinetag encode tag attrs taglist i is_first =
      if taglist = []
      then xh_print_closedtag encode tag attrs i true
      else
        (S.put ("<"^tag)
           ++ xh_print_attrs encode attrs
           ++ S.put ">"
           ++ xh_print_taglist taglist 0 false false
           ++ S.put ("</"^tag^">"))

    and xh_print_blocktag encode tag attrs taglist i =
      if taglist = []
      then xh_print_closedtag encode tag attrs i true
      else
        (S.put (if i > 0 then "\n"^String.make (taille_tab*i) ' ' else "\n")
	  ++ S.put ("<"^tag)
	  ++ xh_print_attrs encode attrs
          ++ S.put  ">"
	  ++ xh_print_taglist_removews taglist (i+1) true
	  ++ S.put (if i > 0 then "\n"^String.make (taille_tab*i) ' ' else "\n")
	  ++ S.put ("</"^tag^">"))

    and xh_print_semiblocktag encode tag attrs taglist i =
      (* New line before and after but not inside, for ex for <pre> *)
      if taglist = []
      then xh_print_closedtag encode tag attrs i true
      else
        (S.put (if i > 0 then "\n"^String.make (taille_tab*i) ' ' else "\n")
	   ++ S.put ("<"^tag)
	   ++ xh_print_attrs encode attrs
           ++ S.put ">"
	   ++ xh_print_taglist taglist 0 false false
	   ++ S.put ("</"^tag^">"))

    and xh_print_taglist_removews taglist i is_first =
      xh_print_taglist taglist i is_first true

    and print_nodes ws1 name xh_attrs xh_taglist ws2 queue i is_first removetailingws =
      (if List.mem name F.blocktags
       then xh_print_blocktag encode name xh_attrs xh_taglist i
       else if List.mem name F.semiblocktags
       then xh_print_semiblocktag encode name xh_attrs xh_taglist i
       else (xh_print_text (encode ws1) i is_first
	       ++ xh_print_inlinetag encode name xh_attrs xh_taglist i is_first
               ++ xh_print_text (encode ws2) i is_first))
       ++ xh_print_taglist queue i false removetailingws

    and xh_print_taglist taglist i is_first removetailingws =
      match taglist with

      | [] -> S.empty ()

      | { elt = Comment texte }::queue ->
          xh_print_text ("<!--"^(encode texte)^"-->") i is_first
          ++ xh_print_taglist queue i false removetailingws

      | { elt = Entity e }::queue ->
          xh_print_text ("&"^e^";") i is_first (* no encoding *)
          ++ xh_print_taglist queue i false removetailingws

      | { elt = PCDATA texte }::queue ->
          xh_print_text (encode texte) i is_first
          ++ xh_print_taglist queue i false removetailingws

      | { elt = EncodedPCDATA texte }::queue ->
          xh_print_text texte i is_first
          ++ xh_print_taglist queue i false removetailingws

      | { elt = Node (name,xh_attrs,xh_taglist )}::queue ->
          print_nodes "" name xh_attrs xh_taglist "" queue i is_first removetailingws

      | { elt = Leaf (name,xh_attrs )}::queue ->
          print_nodes "" name xh_attrs [] "" queue i is_first removetailingws

      | { elt = Empty }::queue ->
          xh_print_taglist queue i false removetailingws

    in
    xh_print_taglist [arbre] 0 true false

  let opt_default x = function
    | Some x -> x
    | _ -> x

  let xhtml_stream
      ?version ?(width=132) ?(encode = encode_unsafe) ?html_compat arbre =
    let version = opt_default F.default_doctype version in
    S.make
      (S.put (F.doctype version)
	 ++ S.put F.ocsigenadv
	 ++ aux ?width ?encode ?html_compat (F.toelt arbre))

  let xhtml_list_stream
      ?(width=132) ?(encode = encode_unsafe) ?html_compat foret =
    S.make
      (List.fold_right (<<)
	 (List.map
	    (fun arbre -> aux ?width ?encode ?html_compat (F.toelt arbre))
	    foret)
	 (S.empty ()))
end




