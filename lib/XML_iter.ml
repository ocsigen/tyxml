(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2004 Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
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

module Make(XML : XML_sigs.Iterable) = struct

  open XML

(** Iterators *)

  let amap1 f n =
    match content n with
    | Empty | Comment _ | PCDATA _ | EncodedPCDATA _ | Entity _ -> n
    | Leaf (name, attribs) -> leaf ~a:(f name attribs) name
    | Node (name, attribs, elts) -> node ~a:(f name attribs) name elts

  let rec amap f n =
    match content n with
    | Empty | Comment _ | PCDATA _ | EncodedPCDATA _ | Entity _ -> n
    | Leaf (name, attribs) -> leaf ~a:(f name attribs) name
    | Node (name, attribs, elts) ->
	node ~a:(f name attribs) name (List.map (amap f) elts)

  let rec add_float_attrib name value = function
    | [] -> [float_attrib name value]
    | head :: tail when aname head = name ->
	float_attrib name value :: tail
    | head :: tail -> head :: add_float_attrib name value tail

  let rec map_float_attrib is_attrib f l =
    let aux head = match acontent head with
    | AFloat (name, value) when is_attrib name -> float_attrib name (f value)
    | _ -> head in
    List.map aux l

  let rec add_int_attrib name value = function
    | [] -> [int_attrib name value]
    | head :: tail when aname head = name ->
	int_attrib name value :: tail
    | head :: tail -> head :: add_int_attrib name value tail

  let rec rm_attrib is_attrib = function
    | [] -> []
    | head :: tail when is_attrib (aname head) -> rm_attrib is_attrib tail
    | head :: tail -> head :: rm_attrib is_attrib tail

  let rec map_int_attrib is_attrib f l =
    let aux head = match acontent head with
    | AInt (name, value) when is_attrib name -> int_attrib name (f value)
    | _ -> head in
    List.map aux l

  let rec add_string_attrib name value = function
    | [] -> [string_attrib name value]
    | head :: tail when aname head = name ->
	string_attrib name value :: tail
    | head :: tail -> head :: add_string_attrib name value tail

  let rec map_string_attrib is_attrib f l =
    let aux head = match acontent head with
    | AStr (name, value) when is_attrib name -> string_attrib name (f value)
    | _ -> head in
    List.map aux l

  let rec add_space_sep_attrib name value = function
    | [] -> [space_sep_attrib name [value]]
    | head :: tail ->
	match acontent head with
	| AStrL (Space, name', values') when name' = name ->
	    space_sep_attrib name (value :: values') :: tail
	| _ when aname head = name ->
	    space_sep_attrib name [value] :: tail
	| _ -> head :: add_space_sep_attrib name value tail

  let rec add_comma_sep_attrib name value = function
    | [] -> [comma_sep_attrib name [value]]
    | head :: tail ->
	match acontent head with
	| AStrL (Comma, name', values') when name' = name ->
	    comma_sep_attrib name (value :: values') :: tail
	| _ when aname head = name ->
	    comma_sep_attrib name [value] :: tail
	| _ -> head :: add_comma_sep_attrib name value tail

  let rec rm_attrib_from_list is_attrib is_value = function
    | [] -> []
    | head :: tail ->
	match acontent head with
	| AStrL (sep, name, values) when is_attrib name ->
	    begin match List.filter (fun v -> not (is_value v)) values with
	    | [] -> tail
	    | values' ->
		match sep with
		| Space -> space_sep_attrib name values' :: tail
		| Comma -> comma_sep_attrib name values' :: tail
	    end
	| _ -> head :: rm_attrib_from_list is_attrib is_value tail

  let rec map_string_attrib_in_list is_attrib f l =
    let aux head = match acontent head with
    | AStrL (sep, name, values) when is_attrib name ->
	begin match sep with
	| Comma -> comma_sep_attrib name (List.map f values)
	| Space -> space_sep_attrib name (List.map f values)
	end
    | _ -> head in
    List.map aux l

  let rec fold of_empty of_comment of_pcdata of_encodedpcdata of_entity
      of_leaf of_node n =
    match content n with
    | Empty -> of_empty ()
    | Comment s -> of_comment s
    | PCDATA s -> of_pcdata s
    | EncodedPCDATA s -> of_encodedpcdata s
    | Entity s -> of_entity s
    | Leaf (name, attribs) -> of_leaf name attribs
    | Node (name, attribs, elts) ->
	of_node name attribs
          (List.map (fold of_empty of_comment of_pcdata of_encodedpcdata of_entity of_leaf of_node) elts)

(* (* is this AT ALL useful??? *)
   let rec foldx of_empty of_comment of_pcdata of_entity of_leaf of_node update_state state = function
   | Empty -> of_empty ()
   | Comment s -> of_comment s
   | PCDATA s -> of_pcdata s
   | Entity s -> of_entity s
   | Leaf (name, attribs) -> of_leaf state name attribs
   | Node (name, attribs, elts) ->
   of_node state name attribs
   (List.map (foldx of_empty of_comment of_pcdata of_entity of_leaf of_node
   update_state (update_state name attribs state)) elts)
 *)

  let all_attribs access ?(is_elt = fun ename -> true) aname elt =
    let access' ename attribs =
      if is_elt ename then
	try [access aname attribs] with Not_found -> []
      else
	[] in
    fold (fun () -> []) (fun c -> []) (fun p -> []) (fun p -> []) (fun e -> []) access'
      (fun ename attribs elts -> access' ename attribs @ List.flatten elts)
      elt

  let all_entities elt =
    fold (fun () -> []) (fun c -> []) (fun p -> []) (fun p -> []) (fun e -> [e])
      (fun ename attribs -> []) (fun ename attribs elts -> List.flatten elts)
      elt

  let rec flatmap f = function
    | [] -> []
    | x :: rest -> f x @ flatmap f rest

  let translate root_leaf root_node sub_leaf sub_node update_state state n =
    let rec translate' state  n =
      match content n with
      | (Empty | Comment _ | PCDATA _ | EncodedPCDATA _ | Entity _) -> [n]
      | Leaf (name, attribs) ->
          sub_leaf state name attribs
      | Node (name, attribs, elts) ->
          sub_node state name attribs
            (flatmap (translate' (update_state name attribs state)) elts)
    in
    match content n with
    | (Empty | Comment _ | PCDATA _ | EncodedPCDATA _ | Entity _) -> n
    | Leaf (name, attribs) ->
	root_leaf name attribs
    | Node (name, attribs, elts) ->
	root_node name attribs (flatmap (translate' state) elts)

end
