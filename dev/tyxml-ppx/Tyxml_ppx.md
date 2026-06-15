
# Module `Tyxml_ppx`

TyXML ppx library.

This is the documentation for the internal ppx library. ` Documentation for the ppx itself is available
    {{!page-"ppx"}here}. `

```ocaml
type lang = 
  | Html
  | Svg
```
```ocaml
val markup_to_expr : 
  lang ->
  Ppxlib.Location.t ->
  Ppxlib.expression list ->
  Ppxlib.expression
```
Given the payload of a `%html ...` or `%svg ...` expression, converts it to a TyXML expression representing the markup contained therein.

```ocaml
val expand_expr : 
  lang:lang ->
  loc:Ppxlib.Location.t ->
  path:string ->
  arg:Ppxlib.Longident.t Ppxlib.Asttypes.loc option ->
  Ppxlib.expression ->
  Ppxlib.attribute list ->
  Ppxlib.expression
```
```ocaml
val expand_str_item : 
  lang:lang ->
  loc:Ppxlib.Location.t ->
  path:string ->
  arg:Ppxlib.Longident.t Ppxlib.Asttypes.loc option ->
  Ppxlib.rec_flag ->
  Ppxlib.value_binding list ->
  Ppxlib.structure_item
```