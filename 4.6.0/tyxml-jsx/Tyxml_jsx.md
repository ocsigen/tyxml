
# Module `Tyxml_jsx`

```ocaml
val is_jsx : Ppxlib.Parsetree.expression -> bool
```
```ocaml
val lowercase_lead : string -> string
```
```ocaml
val to_kebab_case : string -> string
```
```ocaml
val make_attr_name : string -> string
```
```ocaml
val filter_map : ('a -> 'b option) -> 'a list -> 'b list
```
Children

```ocaml
val make_txt : 
  loc:Ppxlib.Location.t ->
  lang:Tyxml_syntax.Common.lang ->
  string ->
  Astlib.Ast_502.Parsetree.expression
```
```ocaml
val element_mapper : 
  (Ppxlib.Parsetree.expression -> Astlib.Ast_502.Parsetree.expression) ->
  Ppxlib.Parsetree.expression ->
  Astlib.Ast_502.Parsetree.expression
```
```ocaml
val extract_element_list : 
  (Ppxlib.Parsetree.expression -> Astlib.Ast_502.Parsetree.expression) ->
  Ppxlib_ast.Ast.expression ->
  Astlib.Ast_502.Parsetree.expression Tyxml_syntax.Common.value list
```
```ocaml
val extract_children : 
  (Ppxlib.Parsetree.expression -> Astlib.Ast_502.Parsetree.expression) ->
  (Ppxlib.Asttypes.arg_label * Ppxlib_ast.Ast.expression) list ->
  Astlib.Ast_502.Parsetree.expression Tyxml_syntax.Common.value list
```
Attributes

```ocaml
type attr = {
  a_name : Tyxml_syntax.Common.name;
  a_value : string Tyxml_syntax.Common.value;
  a_loc : Ppxlib.Location.t;
}
```
```ocaml
val extract_attr_value : 
  lang:'a ->
  string ->
  Ppxlib.Parsetree.expression ->
  ('a * string) * string Tyxml_syntax.Common.value
```
```ocaml
val extract_attr : 
  lang:'a ->
  (Ppxlib.Asttypes.arg_label * Ppxlib_ast.Ast.expression) ->
  (('a * string) * string Tyxml_syntax.Common.value) option
```
```ocaml
val classify_name : 
  loc:Ppxlib.Location.t ->
  Tyxml_syntax.Common.lang option ->
  Ppxlib.Longident.t ->
  Tyxml_syntax.Common.lang * (Tyxml_syntax.Common.lang * string)
```
```ocaml
val is_homemade_component : Ppxlib.Longident.t -> bool
```
```ocaml
val mk_component : 
  lang:Tyxml_syntax.Common.lang ->
  loc:Ppxlib.Location.t ->
  Astlib.Ast_502.Parsetree.expression ->
  (('a * string) * string Tyxml_syntax.Common.value) list ->
  Ppxlib.expression Tyxml_syntax.Common.value list ->
  Astlib.Ast_502.Parsetree.expression
```
```ocaml
val traverse : 
  Tyxml_syntax.Common.lang option Ppxlib.Ast_traverse.map_with_context
```