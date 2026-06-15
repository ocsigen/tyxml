
# Module `Tyxml_svg.Make_printer`

Parametrized stream printer for Svg documents.

deprecated Use pp instead.

## Parameters

```ocaml
module O : Xml_sigs.Output
```

## Signature

```ocaml
val print_list : ?encode:(string -> string) -> 'a elt list -> O.out
```
```ocaml
val print : ?encode:(string -> string) -> ?advert:string -> doc -> O.out
```