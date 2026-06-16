
# Module type `Xml_sigs.Typed_pp`

```ocaml
type 'a elt
```
```ocaml
type doc
```
```ocaml
val pp_elt : 
  ?encode:(string -> string) ->
  ?indent:bool ->
  unit ->
  Stdlib.Format.formatter ->
  'a elt ->
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
  doc ->
  unit
```
`pp ()` is a `Format` printer for complete documents.

It can be used in combination with `"%a"`. For example, to get a string:

```ocaml
let s = Format.asprintf "%a" (pp ()) my_document
```
A custom encoding function can be provided with the `~encode` argument. Various implementations of `encode` are available in [`Xml_print`](./Xml_print.md).
