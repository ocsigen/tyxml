
# Module `Xml_print.Make_typed_simple`

deprecated Use Xml\_print.Make\_typed\_fmt instead.

## Parameters

```ocaml
module Xml : Xml_sigs.Iterable
```
```ocaml
module Typed_xml : Xml_sigs.Typed_xml with module Xml := Xml
```

## Signature

```ocaml
val print_list : 
  output:(string -> unit) ->
  ?encode:(string -> string) ->
  'a Typed_xml.elt list ->
  unit
```
```ocaml
val print : 
  output:(string -> unit) ->
  ?encode:(string -> string) ->
  ?advert:string ->
  Typed_xml.doc ->
  unit
```