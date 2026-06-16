
# Module `Tyxml_syntax.Common`

```ocaml
val find : ('a -> bool) -> 'a list -> 'a option
```
Similar to `List.find`, but evaluates to an option instead of raising `Not_found`.

Markup language

```ocaml
type lang = 
  | Html
  | Svg
```
```ocaml
val lang : lang -> string
```
```ocaml
val implementation : lang -> string
```
```ocaml
val set_implementation : lang -> string -> unit
```
```ocaml
type name = lang * string
```
```ocaml
val make_lid : 
  loc:Ppxlib.Location.t ->
  lang ->
  string ->
  Ppxlib.Longident.t Ppxlib.Location.loc
```
```ocaml
val make : loc:Ppxlib.Location.t -> lang -> string -> Ppxlib.expression
```
Expression helpers.

```ocaml
val int : Ppxlib.Location.t -> int -> Ppxlib.expression
```
```ocaml
val float : Ppxlib.Location.t -> float -> Ppxlib.expression
```
```ocaml
val string : Ppxlib.Location.t -> string -> Ppxlib.expression
```
```ocaml
val list : Ppxlib.Location.t -> Ppxlib.expression list -> Ppxlib.expression
```
```ocaml
val list_wrap : 
  lang ->
  Ppxlib.Location.t ->
  Ppxlib.expression list ->
  Ppxlib.expression
```
```ocaml
val wrap : lang -> Ppxlib.Location.t -> Ppxlib.expression -> Ppxlib.expression
```
`wrap implementation loc e` creates a parse tree for `implementation.Xml.W.return e`.

```ocaml
type 'a value = 
  | Val of 'a
  | Antiquot of Ppxlib.expression
```
```ocaml
val map_value : ('a -> 'b) -> 'a value -> 'b value
```
```ocaml
val value : 'a -> 'a value
```
```ocaml
val antiquot : Ppxlib.expression -> _ value
```
```ocaml
val wrap_value : 
  lang ->
  Ppxlib.Location.t ->
  Ppxlib.expression value ->
  Ppxlib.expression
```
```ocaml
val list_wrap_value : 
  lang ->
  Ppxlib.Location.t ->
  Ppxlib.expression value list ->
  Ppxlib.expression
```
```ocaml
val error : 
  Ppxlib.Location.t ->
  ('b, Stdlib.Format.formatter, unit, 'a) Stdlib.format4 ->
  'b
```
```ocaml
val txt : loc:Ppxlib.Location.t -> lang:lang -> string -> Ppxlib.expression
```