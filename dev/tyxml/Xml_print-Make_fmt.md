
# Module `Xml_print.Make_fmt`

Printers for raw XML modules.


## Parameters

```ocaml
module Xml : Xml_sigs.Iterable
```
```ocaml
module I : TagList
```

## Signature

```ocaml
val pp : 
  ?encode:(string -> string) ->
  ?indent:bool ->
  unit ->
  Stdlib.Format.formatter ->
  Xml.elt ->
  unit
```
`pp ()` is a `Format` printer for untyped XML.

It can be used in combination with `"%a"`. For example, to get a string:

```ocaml
let s = Format.asprintf "%a" (pp ()) my_xml
```
A custom encoding function can be provided with the `~encode` argument. Various implementations of `encode` are available in [`Xml_print`](./Xml_print.md).
