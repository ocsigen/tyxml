module F = Format

module Make
    (S : Svg_sigs.NoWrap)
    (H : Html_sigs.NoWrap
     with module Xml = S.Xml
      and module Svg := S) = struct

  type out = Html_types.phrasing H.elt
  
  type constr = out list -> out

  type level = {
    context : constr ;
    elts : out list ;
  }

  type Format.stag +=
    | Constr of constr
    | Elements of out list
  
  type formatter = {
    ppf : Format.formatter ;
    mutable stack : level list ;
    mutable current : out list ;
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
    let state = {
      stack = [] ;
      current = [] ;
      ppf = make_dummy_ppf ();
    } in

    let out_string s i j = add_element state (H.pcdata @@ String.sub s i j)
    and out_flush () = ()
    and out_newline () = add_element state (H.br ())
    and out_spaces i = add_element state (H.pcdata @@ String.make i ' ')
    and out_indent i = add_element state (H.pcdata @@ String.make i ' ')
    in
    Format.pp_set_formatter_out_functions state.ppf
      {out_string; out_flush; out_newline; out_spaces; out_indent} ;

    let mark_open_stag _ = ""
    and mark_close_stag _ = ""
    and print_open_stag tag = match tag with
      | Constr f -> push state f
      | Elements l -> add_elements state l
      | _ -> ()
    and print_close_stag tag = match tag with
      | Constr _ -> pop state
      | Elements _ -> ()
      | _ -> ()
    in
    Format.pp_set_mark_tags state.ppf true ;
    Format.pp_set_formatter_stag_functions state.ppf
      {mark_open_stag; mark_close_stag; print_open_stag; print_close_stag} ;
    state

  let flush fmt =
    Format.pp_print_flush fmt.ppf ();
    while fmt.stack <> [] do
      pop fmt ;
    done ;
    fmt.current

  let kpr k =
    let fmt = make () in
    Fmt.kpf (fun _ -> k @@ flush fmt) fmt.ppf
  let pr fmt = kpr (fun x -> x) fmt
  
  (** Basic types *)
  
  let with_elements f ppf elts =
    Format.pp_open_stag ppf (Elements elts);
    f ppf elts;
    Format.pp_close_stag ppf ();
    ()
  let elements ppf elts =
    with_elements (fun _ _ -> ()) ppf elts
  let element ppf elt =
    elements ppf [elt]
  let element_sized ppf (i,elt) =
    let f ppf _ = Format.pp_print_as ppf i "" in
    with_elements f ppf [elt]

  let wrapped c f ppf x = 
    Format.pp_open_stag ppf (Constr c);
    f ppf x;
    Format.pp_close_stag ppf ();
    ()
    
  let star (c : _ H.star) ?a f = wrapped (c ?a) f
  let starl c ?a ?sep f = star c ?a (Fmt.list ?sep f)

  let tagf c ppf fmt =
    Format.pp_open_stag ppf (Constr c);
    Fmt.kpf
      (fun ppf -> Format.pp_close_stag ppf ())
      ppf
      fmt
    
  (** Some specific elements. *)
  let u ?a = star H.u ?a
  let b ?a = star H.b ?a
  let span ?a = star H.span ?a
  
  (** Additional type definitions. *)

  type 'a attribs = 'a H.attrib list
end
