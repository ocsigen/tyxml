
# Module type `Xml_sigs.Typed_simple_printer`

```ocaml
type 'a elt
```
```ocaml
type doc
```
```ocaml
val print_list : 
  output:(string -> unit) ->
  ?encode:(string -> string) ->
  'a elt list ->
  unit
```
```ocaml
val print : 
  output:(string -> unit) ->
  ?encode:(string -> string) ->
  ?advert:string ->
  doc ->
  unit
```