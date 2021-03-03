
open Ast_helper
module AC = Ast_convenience

let mustache_from_file file =
  let chan = open_in file in
  let lex = Lexing.from_channel chan in
  Location.init lex file ;
  let t = Mustache.With_locations.parse_lx lex in
  close_in chan ;
  t

let mustache_from_string ~lexloc string =
  let lex = Lexing.from_string string in
  lex.Lexing.lex_start_p <- lexloc ;
  lex.Lexing.lex_curr_p <- lexloc ;
  Mustache.With_locations.parse_lx lex

let antiquot_pcdata ~loc ~lang var =
  let pcdata = Ppx_common.make ~loc lang "pcdata" in
  AC.list [
    Exp.apply ~loc pcdata
      [Ppx_common.Label.nolabel, AC.evar var]
  ]

module Var = struct

  module Env = Map.Make(String)

  type kind =
    | Var
    | Expr
    | Section of kind Env.t

  let pp fmt = function
    | Var -> Format.pp_print_string fmt "variable"
    | Expr -> Format.pp_print_string fmt "unescaped variable"
    | Section _ -> Format.pp_print_string fmt "section"

  let rec equal k k' = match k, k' with
    | Var, Var | Expr, Expr -> true
    | Section env, Section env' ->
      Env.equal equal env env'
    | _, _ -> false

  let error s k k' =
    Location.error @@ Format.asprintf
      "Variable %s is used both as a %a and a %a. This is not allowed."
      s   pp k'   pp k

  let add env s k =
    if Env.mem s env then
      let k' = Env.find s env in
      if equal k k then env
      else raise @@ Location.Error (error s k k')
    else
      Env.add s k env

  let union =
    let f s parentkind kind = match parentkind, kind with
      | Some k, Some k' -> raise @@ Location.Error (error s k k')
      | Some k, None | None, Some k -> Some k
      | None, None -> None
    in Env.merge f

  let section env s secenv =
    let k = Section secenv in
    add env s k

end

module Template = struct

  type t = desc Location.loc list
  and desc =
    | Markup of string
    | Pcdata of string
    | Expr of string
    | Section of section
  and section = {
    inverted : bool;
    name: string;
    contents: t;
  }

  let mkloc {Mustache.With_locations. loc_start ; loc_end } txt =
    let loc = {Location. loc_ghost = true ; loc_start ; loc_end} in
    [{Location. loc ; txt}]

  let rec of_mustache resolve =
    Mustache.With_locations.fold
      ~string:(fun ~loc x -> mkloc loc @@ Markup x)
      ~section:
        (fun ~loc ~inverted name contents ->
           mkloc loc @@ Section { inverted ; name ; contents})
      ~escaped:(fun ~loc x -> mkloc loc @@ Pcdata x)
      ~unescaped:(fun ~loc x -> mkloc loc @@ Expr x)
      ~partial:
        (fun ~loc:_ s ->
           of_mustache resolve @@ mustache_from_file @@ resolve s)
      ~comment:(fun ~loc:_ _ -> [])
      ~concat:(fun ~loc:_ l -> List.concat l)

  let bindings ~env ~sec_env ~id =
    let f s b b' = match b, b' with
      | Some k, Some k' ->
        if Var.equal k k' then None
        else raise @@ Location.Error (Var.error s k k')
      | None, Some k' -> Some k'
      | _, None -> None
    in
    let env = Var.Env.merge f env sec_env in
    let make_binding k _ l =
      Vb.mk (AC.pvar k) (Exp.send id k) :: l
    in
    Exp.let_ Asttypes.Nonrecursive @@ Var.Env.fold make_binding env []

  let rec desc_to_expr ~lang env {Location. txt; loc} =
    Ast_helper.default_loc := loc ;
    match (txt : desc) with
    | Markup s -> env, AC.str s
    | Pcdata s ->
      Var.add env s Var, antiquot_pcdata ~loc ~lang s
    | Expr s ->
      Var.add env s Expr, AC.evar s
    | Section { inverted ; name ; contents } ->
      let sec_env, e =
        to_expr ~simplify:false ~loc ~lang Var.Env.empty contents
      in
      let env = Var.section env name sec_env in
      let id = AC.evar name in
      let pid = AC.pvar name in
      if inverted then
        env, [%expr if [%e id] = [] then [] else [%e e]]
      else
        let e = bindings ~env ~sec_env ~id e in
        env, [%expr List.concat (List.map (fun [%p pid] -> [%e e]) [%e id])]

  and to_expr ~simplify ~loc ~lang env l =
    let f (env, acc) t =
      let env, expr = desc_to_expr ~lang env t in
      env, expr::acc
    in
    let env, l = List.fold_left f (env, []) l in
    env, Ppx_tyxml.markup_to_expr ~simplify lang loc @@ List.rev l

  let make_function env e =
    let f s _k e =
      Exp.fun_ (AC.Label.labelled s) None (AC.pvar s) e
    in
    Var.Env.fold f env e

end

let list_as_app = function
  | [] -> AC.unit ()
  | h :: t -> Exp.apply h (List.map (fun x -> AC.Label.nolabel, x) t)

let expr_of_mustache ~loc ~lang t =
  let env, e =
    Template.to_expr ~simplify:true ~loc ~lang Var.Env.empty @@
    Template.of_mustache (fun _s -> assert false) @@
    t
  in
  Template.make_function env e

let expr_of_string ~loc ~lang ~lexloc s =
  expr_of_mustache ~loc ~lang @@
  mustache_from_string ~lexloc s


(** Mappers *)

open Parsetree

let error loc =
  Ppx_common.error loc "Invalid payload for [%%template]."

let extract_str loc str =
  match AC.get_str_with_quotation_delimiter str with
  | None -> error loc
  | Some (s,quot) -> (Ppx_tyxml.Loc.string_start quot loc, s)

let expr mapper e =
  let sloc = e.pexp_loc in
  match e.pexp_desc with
  | Pexp_extension ({ txt = ("template" | "tyxml.template")}, payload) ->
    begin match payload with
      | PStr [[%stri let [%p? var] = [%e? str] in [%e? e]]] ->
        let loc = str.pexp_loc in
        let lexloc, s = extract_str loc str in
        Exp.let_ ~loc:sloc Asttypes.Nonrecursive
          [Vb.mk ~loc:sloc var @@
           expr_of_string ~loc ~lang:Html ~lexloc s]
          e

      | PStr [{pstr_desc = Pstr_eval (str, _)}] ->
        let loc = str.pexp_loc in
        let lexloc, s = extract_str loc str in
        expr_of_string ~loc ~lang:Html ~lexloc s

      | _ -> error sloc
    end
  | _ -> Ast_mapper.default_mapper.expr mapper e

let structure_item mapper stri =
  let sloc = stri.pstr_loc in
  match stri.pstr_desc with
  | Pstr_extension (({ txt = ("template" | "tyxml.template")}, payload), _) ->
    begin match payload with
      | PStr [([%stri let [%p? var] = [%e? str]] as decl)] ->
        let loc = str.pexp_loc in
        let lexloc, s = extract_str loc str in
        Str.value ~loc:decl.pstr_loc Asttypes.Nonrecursive
          [Vb.mk ~loc:decl.pstr_loc var @@
           expr_of_string ~loc ~lang:Html ~lexloc s]
      | _ -> error sloc
    end
  | _ -> Ast_mapper.default_mapper.structure_item mapper stri

let mapper _ =
  { Ast_mapper. default_mapper with expr ; structure_item }

let () = Ast_mapper.register "tyxml.template" mapper
