
# Module `Xml_stream`

Streaming IO to/from XML trees

```ocaml
type name = string * string
```

### Input

```ocaml
type signal = [ 
  | `Comment of string
  | `End_element
  | `Start_element of name * (name * string) list
  | `Text of string list
 ]
```
```ocaml
exception Malformed_stream
```
```ocaml
module Import (Xml : Xml_sigs.T) : sig ... end
```