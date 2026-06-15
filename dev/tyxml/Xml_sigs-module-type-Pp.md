
# Module type `Xml_sigs.Pp`

```ocaml
type elt
```
```ocaml
val pp : 
  ?encode:(string -> string) ->
  ?indent:bool ->
  unit ->
  Stdlib.Format.formatter ->
  elt ->
  unit
```
`pp ()` is a `Format` printer for untyped XML.

It can be used in combination with `"%a"`. For example, to get a string:

```ocaml
let s = Format.asprintf "%a" (pp ()) my_xml
```
A custom encoding function can be provided with the `~encode` argument. Various implementations of `encode` are available in [`Xml_print`](./Xml_print.md).
