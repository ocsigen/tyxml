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

open Asttypes
open Parsetree


module Loc = struct

  let shift (pos:Lexing.position) x = {pos with pos_cnum = pos.pos_cnum + x}

  (** Returns the real (OCaml) location of a string, taking delimiters into
      account. *)
  let string_start delimiter loc =
    let delimiter_length = match delimiter with
      | None -> 1
      | Some d -> String.length d + 2
    in
    shift loc.Location.loc_start delimiter_length

  (** Converts a Markup.ml input location into an OCaml location. [loc] is the
      start of the OCaml location of the string being parsed by Markup.ml.
      [consumed] is the number of bytes consumed by Markup.ml before the
      beginning of the current string.
      [(line, column)] is the Markup.ml location to be converted. *)
  let adjust loc consumed (line, column) =
    let open Location in
    let open Lexing in

    let column =
      if line <> 1 then column
      else loc.pos_cnum - loc.pos_bol + column - consumed
    in
    let line = loc.pos_lnum + line - 1 in

    let position =
      {pos_fname = loc.pos_fname;
       pos_lnum  = line;
       pos_bol   = 0;
       pos_cnum  = column};
    in

    {loc_start = position;
     loc_end = position;
     loc_ghost = false}

end

(** Antiquotations

    We replace antiquotations expressions by a dummy token "(tyxmlX)".
    We store a table token to expression to retrieve them after parsing.
*)
module Antiquot = struct

  let fmt_id = Printf.sprintf "(tyxml%i)"
  let regex_id = Re.(seq [ str "(tyxml" ; rep digit ; char ')' ])
  let re_id = Re.compile regex_id
  let whole_re_id = Re.(compile @@ whole_string regex_id)

  let make_id =
    let r = ref 0 in
    fun () -> incr r ; fmt_id !r

  module H = Hashtbl.Make(struct
      type t = string
      let hash = Hashtbl.hash
      let equal (x:string) y = x = y
    end)

  let tbl = H.create 17

  let create expr =
    let s = make_id () in
    H.add tbl s expr ;
    s

  let get loc s =
    if H.mem tbl s then H.find tbl s
    else
      Ppx_common.error loc
        "Internal error: This expression placeholder is not registered."

  let mem s = H.mem tbl s

  let contains loc s = match Re.exec_opt re_id s with
    | None -> `No
    | Some g ->
      let (i,j) = Re.Group.offset g 0 in
      let is_whole = i = 0 && j = String.length s in
      if is_whole
      then `Whole (get loc s)
      else `Yes (get loc @@ Re.Group.get g 0)

  let assert_no_antiquot ~loc kind (_namespace,s) =
    match contains loc s with
    | `No -> ()
    | `Yes e | `Whole e ->
      Ppx_common.error e.pexp_loc
        "OCaml expressions are not accepted as %s names." kind

end

(** Building block to rebuild the output with expressions intertwined. *)

let make_pcdata ~loc s =
  [%expr pcdata [%e Ppx_common.string loc s]][@metaloc loc]

(** Walk the text list to replace placeholders by OCaml expressions when
    appropriate. Use {!make_pcdata} on the rest. *)
let make_text ~loc ss =
  let buf = Buffer.create 17 in
  let push_pcdata buf l =
    let s = Buffer.contents buf in
    Buffer.clear buf ;
    if s = "" then l else make_pcdata ~loc s :: l
  in
  let rec aux ~loc res = function
    | [] -> push_pcdata buf res
    | `Text s :: t ->
        Buffer.add_string buf s ;
        aux ~loc res t
    | `Delim g :: t ->
      let e = Antiquot.get loc @@ Re.get g 0 in
      aux ~loc (e :: push_pcdata buf res) t
  in
  aux ~loc [] @@ Re.split_full Antiquot.re_id @@ String.concat "" ss

let replace_attribute ~loc (attr,value) =
  Antiquot.assert_no_antiquot ~loc "attribute" attr ;
  match Antiquot.contains loc value with
  | `No -> (attr, `String value)
  | `Whole e -> (attr, `Expr e)
  | `Yes _ ->
      Ppx_common.error loc
      "Mixing literals and OCaml expressions is not authorized in attribute values."


(** Processing *)

(** Takes the ast and transforms it into a Markup.ml char stream.

    The payload [expr] is either a single token, or an application (that is, a list).
    A token is either a string or an antiquotation, which is transformed into
    a string (see {!Antiquot}).

    Each token is equipped with a starting (but no ending) position.
*)
let ast_to_stream expr =
  let current_adjust_location = ref (Loc.adjust Lexing.dummy_pos 0) in

  let expressions =
    match expr.pexp_desc with
    | Pexp_apply (f, arguments) -> f::(List.map snd arguments)
    | _ -> [expr]
  in

  let strings =
    expressions |> List.map @@ fun expr ->
    match expr.pexp_desc with
    (* TODO: Doesn't work in 4.03, can't pattern match. *)
    | Pexp_constant (Const_string (s, delimiter)) ->
      (s, Loc.string_start delimiter expr.pexp_loc)
    | _ ->
      (Antiquot.create expr, expr.pexp_loc.loc_start)
  in

  let items = ref strings in
  let offset = ref 0 in
  let consumed = ref 0 in

  let rec next () = match !items with
    | [] -> None
    | (s, loc)::rest ->
      if !offset = 0 then begin
        current_adjust_location := Loc.adjust loc !consumed;
        consumed := !consumed + String.length s
      end;

      if !offset < String.length s then begin
        offset := !offset + 1;
        Some (s.[!offset - 1])
      end
      else begin
        offset := 0;
        items := rest;
        next ()
      end
  in

  Markup.fn next, (fun x -> !current_adjust_location x)

(** Given the payload of a [%tyxml ...] expression, converts it to a TyXML
    expression representing the markup contained therein. *)
let markup_to_expr loc expr =

  let input_stream, adjust_location = ast_to_stream expr in

  (* The encoding is specified as a workaround: when not specified, Markup.ml
     prescans the input looking for byte-order marks or <meta> tags. We don't
     want a prescan, because that will trigger premature insertion of literal
     TyXML expressions into the initial, empty, child list, by the input stream,
     before the expression assembler starts running. This is fragile and will be
     fixed by merging TyXML expressions in the assembler instead of as now. *)
  let parser =
    Markup.parse_html
      ~encoding:Markup.Encoding.utf_8
      ~report:(fun loc error ->
        let loc = adjust_location loc in
        let message = Markup.Error.to_string error |> String.capitalize in
        Ppx_common.error loc "%s" message)
      input_stream
  in
  let signals = Markup.signals parser in
  let get_loc () = adjust_location @@ Markup.location parser in

  let rec assemble children =
    match Markup.next signals with
    | None | Some `End_element -> List.rev children

    | Some (`Text ss) ->
      let loc = get_loc () in
      let node = make_text ~loc ss in
      assemble (node @ children)

    | Some (`Start_element (name, attributes)) ->
      let loc = get_loc () in

      let sub_children = assemble [] in
      Antiquot.assert_no_antiquot ~loc "element" name ;
      let attributes = List.map (replace_attribute ~loc) attributes in
      let node = Ppx_element.parse ~loc ~name ~attributes sub_children in
      assemble (node :: children)

    | Some _ ->
      assemble children
  in

  Ppx_common.list loc @@ assemble []



open Ast_mapper

let map_expr mapper e =
  match e.pexp_desc with
  | Pexp_extension ({txt = "tyxml"; loc}, payload) ->
    begin match payload with
    | PStr [{pstr_desc = Pstr_eval (e, _)}] ->
      markup_to_expr loc e
    | _ ->
      Ppx_common.error e.pexp_loc
        "Error: Payload of [%%tyxml] must be a single string"
    end
  | _ -> default_mapper.expr mapper e



let () =
  register "tyxml" (fun _ -> {default_mapper with expr = map_expr})
