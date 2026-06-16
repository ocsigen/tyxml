
# Module type `Xml_sigs.Simple_printer`

```ocaml
type xml_elt
```
```ocaml
val print_list : 
  output:(string -> unit) ->
  ?encode:(string -> string) ->
  xml_elt list ->
  unit
```