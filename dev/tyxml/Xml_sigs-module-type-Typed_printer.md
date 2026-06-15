
# Module type `Xml_sigs.Typed_printer`

```ocaml
type 'a elt
```
```ocaml
type doc
```
```ocaml
type out
```
```ocaml
val print_list : ?encode:(string -> string) -> 'a elt list -> out
```
```ocaml
val print : ?encode:(string -> string) -> ?advert:string -> doc -> out
```