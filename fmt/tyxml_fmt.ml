module F = Format

module Make (H : Html_sigs.NoWrap) = struct

  type out = Html_types.phrasing H.elt
  
  type constr = out list -> out

  type level = {
    context : constr ;
    elts : out list ;
  }

  module Tags = struct
    type elt =
      | Constr of constr
      | Elements of out list
    let to_string (x : elt) = string_of_int @@ Hashtbl.hash x 

    let hash = Hashtbl.hash
    module Tbl = Hashtbl.Make(struct
        type t = Format.tag
        let equal = String.equal
        let hash = hash
      end)

    type t = elt Tbl.t
    let create = Tbl.create
    let find tbl tag : elt =
      try Tbl.find tbl tag with
      | Not_found ->
        invalid_arg "Tyxml_fmt: tags are not allowed in tyxml format strings."
    let add tbl (f: elt) =
      let s = to_string f in
      Tbl.add tbl s f;
      s
  end

  type formatter = {
    mutable stack : level list ;
    mutable current : out list ;
    ppf : Format.formatter ;
    tbl : Tags.t
  }
  
  let push fmt
      (context : _ H.elt list -> _ H.elt) =
    let context = (context :> constr) in
    let elts = fmt.current in
    fmt.stack <- { context ; elts } :: fmt.stack ;
    fmt.current <- [] ;
    ()

  let add_elements fmt elt =
    fmt.current <- elt @ fmt.current
  let add_element fmt elt = add_elements fmt [elt]
  
  let pop fmt =
    let content = List.rev fmt.current in
    begin match fmt.stack with
      | [] -> invalid_arg "Empty stack"
      | { context; elts } :: st ->
        fmt.stack <- st ;
        fmt.current <- context content :: elts ;
    end

  let make_dummy_ppf () =
    F.make_formatter (fun _ _ _ -> ()) ignore

  let make () =
    let fmt = {
      stack = [] ;
      current = [] ;
      ppf = make_dummy_ppf () ;
      tbl = Tags.create 17 ;
    } in

    let out_string s i j = add_element fmt (H.pcdata @@ String.sub s i j)
    and out_flush () = ()
    and out_newline () = add_element fmt (H.br ())
    and out_spaces i = add_element fmt (H.pcdata @@ String.make i ' ')
    and out_indent i = add_element fmt (H.pcdata @@ String.make i ' ')
    in
    Format.pp_set_formatter_out_functions fmt.ppf
      {out_string; out_flush; out_newline; out_spaces; out_indent} ;

    let mark_open_tag _ = ""
    and mark_close_tag _ = ""
    and print_open_tag tag =
      match Tags.find fmt.tbl tag with
      | Tags.Constr f -> push fmt f
      | Tags.Elements l -> add_elements fmt l
    and print_close_tag tag =
      match Tags.find fmt.tbl tag with
      | Tags.Constr _ -> pop fmt
      | Tags.Elements _ -> ()
    in
    Format.pp_set_mark_tags fmt.ppf true ;
    Format.pp_set_formatter_tag_functions fmt.ppf
      {mark_open_tag; mark_close_tag; print_open_tag; print_close_tag} ;
    fmt

  let flush fmt =
    Format.pp_print_flush fmt.ppf ();
    while fmt.stack <> [] do
      pop fmt ;
    done ;
    fmt.current

  (* This is where it gets a bit crazy. *)
  module Magic = struct 
    open CamlinternalFormat
    open CamlinternalFormatBasics

    let pp_gen_box ppf i bty =
      match bty with
      | Pp_hbox -> F.pp_open_hbox ppf ()
      | Pp_vbox -> F.pp_open_vbox ppf i
      | Pp_hvbox -> F.pp_open_hvbox ppf i
      | Pp_hovbox -> F.pp_open_hovbox ppf i
      | Pp_box -> F.pp_open_box ppf i
      | Pp_fits -> assert false

    let rec map f (acc : _ acc) = match acc with
      | Acc_formatting_lit (a, b) -> Acc_formatting_lit (map f a, b)
      | Acc_formatting_gen (a, b) -> Acc_formatting_gen (map f a, map_gen f b)
      | Acc_string_literal (a, b) -> Acc_string_literal (map f a, b)
      | Acc_char_literal (a, b) -> Acc_char_literal (map f a, b)
      | Acc_data_string (a, b) -> Acc_data_string (map f a, b)
      | Acc_data_char (a, b) -> Acc_data_char (map f a, b)
      | Acc_delay (a, b) -> Acc_delay (map f a, f b)
      | Acc_flush a -> Acc_flush (map f a)
      | Acc_invalid_arg (a, b) -> Acc_invalid_arg (map f a, b)
      | End_of_acc -> End_of_acc
    and map_gen f = function
      | Acc_open_tag acc -> Acc_open_tag (map f acc)
      | Acc_open_box acc -> Acc_open_box (map f acc)
    let copy x = map (fun _ -> invalid_arg "lol wat, fuck no") x
    
    let compute_tag tag_acc =
      let buf = Buffer.create 16 in
      strput_acc buf @@ copy tag_acc;
      let len = Buffer.length buf in
      if len < 2 then Buffer.contents buf
      else Buffer.sub buf 1 (len - 2)

    let output_formatting_lit {ppf ; _ } fmting_lit = match fmting_lit with
      | Close_box                 -> F.pp_close_box ppf ()
      | Close_tag                 -> F.pp_close_tag ppf ()
      | Break (_, width, offset)  -> F.pp_print_break ppf width offset
      | FFlush                    -> F.pp_print_flush ppf ()
      | Force_newline             -> F.pp_force_newline ppf ()
      | Flush_newline             -> F.pp_print_newline ppf ()
      | Magic_size (_, _)         -> ()
      | Escaped_at                -> F.pp_print_char ppf '@'
      | Escaped_percent           -> F.pp_print_char ppf '%'
      | Scan_indic c              -> F.pp_print_char ppf '@'; F.pp_print_char ppf c

    let rec output_acc ppf acc = match acc with
      | Acc_string_literal (Acc_formatting_lit (p, Magic_size (_, size)), s)
      | Acc_data_string (Acc_formatting_lit (p, Magic_size (_, size)), s) ->
        output_acc ppf p;
        F.pp_print_as ppf.ppf size s;
      | Acc_char_literal (Acc_formatting_lit (p, Magic_size (_, size)), c)
      | Acc_data_char (Acc_formatting_lit (p, Magic_size (_, size)), c) ->
        output_acc ppf p;
        F.pp_print_as ppf.ppf size (String.make 1 c);
      | Acc_formatting_lit (p, f) ->
        output_acc ppf p;
        output_formatting_lit ppf f;
      | Acc_formatting_gen (p, Acc_open_tag acc') ->
        output_acc ppf p;
        F.pp_open_tag ppf.ppf (compute_tag acc')
      | Acc_formatting_gen (p, Acc_open_box acc') ->
        output_acc ppf p;
        let (indent, bty) = open_box_of_string (compute_tag acc') in
        pp_gen_box ppf.ppf indent bty
      | Acc_string_literal (p, s)
      | Acc_data_string (p, s)   -> output_acc ppf p; F.pp_print_string ppf.ppf s;
      | Acc_char_literal (p, c)
      | Acc_data_char (p, c)     -> output_acc ppf p; F.pp_print_char ppf.ppf c;
      | Acc_delay (p, f)         -> output_acc ppf p; f ppf;
      | Acc_flush p              -> output_acc ppf p; F.pp_print_flush ppf.ppf ();
      | Acc_invalid_arg (p, msg) -> output_acc ppf p; invalid_arg msg;
      | End_of_acc               -> ()


    let kfprintf k ppf (Format (fmt, _)) =
      make_printf
        (fun ppf acc -> output_acc ppf acc; k ppf)
        ppf
        End_of_acc
        fmt

  end

  include Fmt.Make(struct
      type nonrec formatter = formatter
      let kfprintf = Magic.kfprintf
    end)
  let kpr k = kpf (fun ppf -> k @@ flush ppf) (make ())
  let pr fmt = kpr (fun x -> x) fmt
  
  (** Basic types *)
  
  let from f ppf x = f ppf.ppf x 
  let string = from Format.pp_print_string 
  let int = from Format.pp_print_int
  let cut = from Format.pp_print_cut

  let rec list ?(sep = cut) pp_v ppf = function
    | [] -> ()
    | [v] -> pp_v ppf v
    | v :: vs ->
      pp_v ppf v;
      sep ppf ();
      list ~sep pp_v ppf vs

  let with_elements f ppf elts =
    let tag = Tags.add ppf.tbl (Elements elts) in
    Format.pp_open_tag ppf.ppf tag;
    f ppf elts;
    Format.pp_close_tag ppf.ppf ();
    ()
  let elements ppf elts =
    with_elements (fun _ _ -> ()) ppf elts
  let element ppf elt =
    elements ppf [elt]
  let element_sized ppf (i,elt) =
    let f ppf _ = Format.pp_print_as ppf.ppf i "" in
    with_elements f ppf [elt]

  let wrapped c f ppf x = 
    let tag = Tags.add ppf.tbl (Constr c) in
    Format.pp_open_tag ppf.ppf tag;
    f ppf x;
    Format.pp_close_tag ppf.ppf ();
    ()
    
  let star (c : _ H.star) ?a f = wrapped (c ?a) f
  let starl c ?a ?sep f = star c ?a (list ?sep f)

  let tagf c ppf fmt =
    let tag = Tags.add ppf.tbl (Constr c) in
    Format.pp_open_tag ppf.ppf tag;
    kpf
      (fun ppf -> Format.pp_close_tag ppf.ppf ())
      ppf
      fmt
    
  (** Some specific elements. *)
  let u ?a = star H.u ?a
  let b ?a = star H.b ?a
  let span ?a = star H.span ?a
  
  (** Additional type definitions. *)

  type 'a attribs = 'a H.attrib list
end
