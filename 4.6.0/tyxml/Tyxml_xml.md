
# Module `Tyxml_xml`

Basic functions for construction and manipulation of XML tree.

```ocaml
module W = Xml_wrap.NoWrap
```
```ocaml
type 'a wrap = 'a W.t
```
```ocaml
type 'a list_wrap = 'a W.tlist
```
```ocaml
type uri = string
```
```ocaml
val string_of_uri : (uri, string) W.ft
```
```ocaml
val uri_of_string : (string, uri) W.ft
```
```ocaml
type aname = string
```
```ocaml
type event_handler = string
```
```ocaml
type mouse_event_handler = string
```
```ocaml
type keyboard_event_handler = string
```
```ocaml
type touch_event_handler = string
```
```ocaml
type attrib
```
```ocaml
val float_attrib : aname -> float wrap -> attrib
```
```ocaml
val int_attrib : aname -> int wrap -> attrib
```
```ocaml
val string_attrib : aname -> string wrap -> attrib
```
```ocaml
val space_sep_attrib : aname -> string list wrap -> attrib
```
```ocaml
val comma_sep_attrib : aname -> string list wrap -> attrib
```
```ocaml
val event_handler_attrib : aname -> event_handler -> attrib
```
```ocaml
val mouse_event_handler_attrib : aname -> mouse_event_handler -> attrib
```
```ocaml
val keyboard_event_handler_attrib : aname -> keyboard_event_handler -> attrib
```
```ocaml
val touch_event_handler_attrib : aname -> touch_event_handler -> attrib
```
```ocaml
val uri_attrib : aname -> uri wrap -> attrib
```
```ocaml
val uris_attrib : aname -> uri list wrap -> attrib
```
```ocaml
type elt
```
```ocaml
type ename = string
```
```ocaml
val empty : unit -> elt
```
```ocaml
val comment : string -> elt
```
```ocaml
val pcdata : string wrap -> elt
```
```ocaml
val encodedpcdata : string wrap -> elt
```
```ocaml
val entity : string -> elt
```
```ocaml
val leaf : ?a:attrib list -> ename -> elt
```
```ocaml
val node : ?a:attrib list -> ename -> elt list_wrap -> elt
```
```ocaml
val cdata : string -> elt
```
```ocaml
val cdata_script : string -> elt
```
```ocaml
val cdata_style : string -> elt
```
```ocaml
type separator = 
  | Space
  | Comma
```
```ocaml
val aname : attrib -> aname
```
```ocaml
type acontent = private 
  | AFloat of float
  | AInt of int
  | AStr of string
  | AStrL of separator * string list
```
```ocaml
val acontent : attrib -> acontent
```
```ocaml
type econtent = private 
  | Empty
  | Comment of string
  | EncodedPCDATA of string
  | PCDATA of string
  | Entity of string
  | Leaf of ename * attrib list
  | Node of ename * attrib list * elt list
```
```ocaml
val content : elt -> econtent
```
```ocaml
val pp : 
  ?encode:(string -> string) ->
  ?indent:bool ->
  unit ->
  Stdlib.Format.formatter ->
  elt ->
  unit
```
`pp ()` is a `Format` printer for untyped XML.

It can be used in combination with `"%a"`. For example, to get a string:

```ocaml
let s = Format.asprintf "%a" (pp ()) my_xml
```
A custom encoding function can be provided with the `~encode` argument. Various implementations of `encode` are available in [`Xml_print`](./Xml_print.md).


### Import/Export

```ocaml
val of_seq : Xml_stream.signal Stdlib.Seq.t -> elt list
```

### Iterators

```ocaml
val amap : (ename -> attrib list -> attrib list) -> elt -> elt
```
Recursively edit attributes for the element and all its children.

```ocaml
val amap1 : (ename -> attrib list -> attrib list) -> elt -> elt
```
Edit attributes only for one element.

The following can safely be exported by higher level libraries, because removing an attribute from a element is always legal.

```ocaml
val rm_attrib : (aname -> bool) -> attrib list -> attrib list
```
```ocaml
val rm_attrib_from_list : 
  (aname -> bool) ->
  (string -> bool) ->
  attrib list ->
  attrib list
```
```ocaml
val map_int_attrib : 
  (aname -> bool) ->
  (int -> int) ->
  attrib list ->
  attrib list
```
```ocaml
val map_string_attrib : 
  (aname -> bool) ->
  (string -> string) ->
  attrib list ->
  attrib list
```
```ocaml
val map_string_attrib_in_list : 
  (aname -> bool) ->
  (string -> string) ->
  attrib list ->
  attrib list
```
Exporting the following by higher level libraries would drive a hole through a type system, because they allow to add *any* attribute to *any* element.

```ocaml
val add_int_attrib : aname -> int -> attrib list -> attrib list
```
```ocaml
val add_string_attrib : aname -> string -> attrib list -> attrib list
```
```ocaml
val add_comma_sep_attrib : aname -> string -> attrib list -> attrib list
```
```ocaml
val add_space_sep_attrib : aname -> string -> attrib list -> attrib list
```
```ocaml
val fold : 
  (unit -> 'a) ->
  (string -> 'a) ->
  (string -> 'a) ->
  (string -> 'a) ->
  (string -> 'a) ->
  (ename -> attrib list -> 'a) ->
  (ename -> attrib list -> 'a list -> 'a) ->
  elt ->
  'a
```
```ocaml
val all_entities : elt -> string list
```
```ocaml
val translate : 
  (ename -> attrib list -> elt) ->
  (ename -> attrib list -> elt list -> elt) ->
  ('state -> ename -> attrib list -> elt list) ->
  ('state -> ename -> attrib list -> elt list -> elt list) ->
  (ename -> attrib list -> 'state -> 'state) ->
  'state ->
  elt ->
  elt
```

### Deprecated printers

```ocaml
val print_list : 
  output:(string -> unit) ->
  ?encode:(string -> string) ->
  elt list ->
  unit
```
deprecated Use Xml.pp instead.
```ocaml
val print : Stdlib.Format.formatter -> elt -> unit
```
deprecated Use Xml.pp instead.