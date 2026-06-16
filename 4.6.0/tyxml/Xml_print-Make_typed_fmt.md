
# Module `Xml_print.Make_typed_fmt`

Printers for typed XML modules such as the one produced by [`Svg_f`](./Svg_f.md) and [`Html_f`](./Html_f.md).


## Parameters

```ocaml
module Xml : Xml_sigs.Iterable
```
```ocaml
module Typed_xml : Xml_sigs.Typed_xml with module Xml := Xml
```

## Signature

```ocaml
val pp_elt : 
  ?encode:(string -> string) ->
  ?indent:bool ->
  unit ->
  Stdlib.Format.formatter ->
  'a Typed_xml.elt ->
  unit
```
`pp_elt ()` is a `Format` printer for individual elements.

A custom encoding function can be provided with the `~encode` argument. Various implementations of `encode` are available in [`Xml_print`](./Xml_print.md).

```ocaml
val pp : 
  ?encode:(string -> string) ->
  ?indent:bool ->
  ?advert:string ->
  unit ->
  Stdlib.Format.formatter ->
  Typed_xml.doc ->
  unit
```
`pp ()` is a `Format` printer for complete documents.

It can be used in combination with `"%a"`. For example, to get a string:

```ocaml
let s = Format.asprintf "%a" (pp ()) my_document
```
A custom encoding function can be provided with the `~encode` argument. Various implementations of `encode` are available in [`Xml_print`](./Xml_print.md).
