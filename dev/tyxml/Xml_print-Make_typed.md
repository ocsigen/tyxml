
# Module `Xml_print.Make_typed`

deprecated Use Xml\_print.Make\_typed\_fmt instead.

## Parameters

```ocaml
module Xml : Xml_sigs.Iterable
```
```ocaml
module Typed_xml : Xml_sigs.Typed_xml with module Xml := Xml
```
```ocaml
module O : Xml_sigs.Output
```

## Signature

```ocaml
val print_list : ?encode:(string -> string) -> 'a Typed_xml.elt list -> O.out
```
```ocaml
val print : 
  ?encode:(string -> string) ->
  ?advert:string ->
  Typed_xml.doc ->
  O.out
```