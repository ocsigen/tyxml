
# Module `Tyxml_syntax.Element`

Element parsing.

```ocaml
val parse : 
  loc:Ppxlib.Location.t ->
  parent_lang:Common.lang ->
  name:Common.name ->
  attributes:(Common.name * string Common.value) list ->
  Ppxlib.expression Common.value list ->
  Ppxlib.expression
```
`parse ~loc ~parent_lang ~name ~attributes children` evaluates to a parse tree for applying the TyXML function corresponding to element `name` to suitable arguments representing `attributes` and `children`.

```ocaml
val comment : 
  loc:Ppxlib.Location.t ->
  lang:Common.lang ->
  string ->
  Ppxlib.expression
```
`comment ~loc ~ns s` evaluates to a parse tree that represents an XML comment.

```ocaml
val find_assembler : Common.name -> (string * Element_content.assembler) option
```