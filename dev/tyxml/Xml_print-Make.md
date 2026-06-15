
# Module `Xml_print.Make`

deprecated Use Xml\_print.Make\_fmt instead.

## Parameters

```ocaml
module Xml : Xml_sigs.Iterable
```
```ocaml
module I : TagList
```
```ocaml
module O : Xml_sigs.Output
```

## Signature

```ocaml
val print_list : ?encode:(string -> string) -> Xml.elt list -> O.out
```