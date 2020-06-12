open Ppxlib

let str_item_extension name expand =
  Extension.declare_with_path_arg
    name
    Extension.Context.structure_item
    Ast_pattern.(pstr ((pstr_value __ __) ^:: nil))
    expand

let html_str_item_rule = str_item_extension "tyxml.html" Tyxml_ppx.expand_html_str_item |> Ppxlib.Context_free.Rule.extension
let svg_str_item_rule = str_item_extension "tyxml.svg" Tyxml_ppx.expand_svg_str_item |> Ppxlib.Context_free.Rule.extension

let expr_expansion name expand =
Extension.declare_with_path_arg
  name
  Extension.Context.expression
  Ast_pattern.(pstr ((pstr_eval __ __) ^:: nil))
  expand

let html_expr_rule = expr_expansion "tyxml.html" Tyxml_ppx.expand_html_expr |> Ppxlib.Context_free.Rule.extension
let svg_expr_rule = expr_expansion "tyxml.svg" Tyxml_ppx.expand_svg_expr |> Ppxlib.Context_free.Rule.extension

let () =
Ppxlib.Driver.register_transformation
  ~rules:[html_expr_rule; html_str_item_rule; svg_expr_rule; svg_str_item_rule]
  "tyxml"
