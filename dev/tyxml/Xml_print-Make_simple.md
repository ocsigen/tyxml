# Module `Xml_print.Make_simple`

deprecated Use Xml\_print.Make\_fmt instead.

## Parameters

```ocaml
module Xml : Xml_sigs.Iterable
```
```ocaml
module _ : TagList
```

## Signature

```ocaml
val print_list : 
  output:(string -> unit) ->
  ?encode:(string -> string) ->
  Xml.elt list ->
  unit
```
