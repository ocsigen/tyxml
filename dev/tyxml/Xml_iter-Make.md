
# Module `Xml_iter.Make`


## Parameters

```ocaml
module Xml : Xml_sigs.Iterable
```

## Signature

```ocaml
val amap : 
  (Xml.ename -> Xml.attrib list -> Xml.attrib list) ->
  Xml.elt ->
  Xml.elt
```
Recursively edit attributes for the element and all its children.

```ocaml
val amap1 : 
  (Xml.ename -> Xml.attrib list -> Xml.attrib list) ->
  Xml.elt ->
  Xml.elt
```
Edit attributes only for one element.

The following can safely be exported by higher level libraries, because removing an attribute from a element is always legal.

```ocaml
val rm_attrib : (Xml.aname -> bool) -> Xml.attrib list -> Xml.attrib list
```
```ocaml
val rm_attrib_from_list : 
  (Xml.aname -> bool) ->
  (string -> bool) ->
  Xml.attrib list ->
  Xml.attrib list
```
```ocaml
val map_int_attrib : 
  (Xml.aname -> bool) ->
  (int -> int) ->
  Xml.attrib list ->
  Xml.attrib list
```
```ocaml
val map_float_attrib : 
  (Xml.aname -> bool) ->
  (float -> float) ->
  Xml.attrib list ->
  Xml.attrib list
```
```ocaml
val map_string_attrib : 
  (Xml.aname -> bool) ->
  (string -> string) ->
  Xml.attrib list ->
  Xml.attrib list
```
```ocaml
val map_string_attrib_in_list : 
  (Xml.aname -> bool) ->
  (string -> string) ->
  Xml.attrib list ->
  Xml.attrib list
```
Exporting the following by higher level libraries would drive a hole through a type system, because they allow to add *any* attribute to *any* element.

```ocaml
val add_int_attrib : Xml.aname -> int -> Xml.attrib list -> Xml.attrib list
```
```ocaml
val add_float_attrib : Xml.aname -> float -> Xml.attrib list -> Xml.attrib list
```
```ocaml
val add_string_attrib : 
  Xml.aname ->
  string ->
  Xml.attrib list ->
  Xml.attrib list
```
```ocaml
val add_comma_sep_attrib : 
  Xml.aname ->
  string ->
  Xml.attrib list ->
  Xml.attrib list
```
```ocaml
val add_space_sep_attrib : 
  Xml.aname ->
  string ->
  Xml.attrib list ->
  Xml.attrib list
```
```ocaml
val fold : 
  (unit -> 'a) ->
  (string -> 'a) ->
  (string -> 'a) ->
  (string -> 'a) ->
  (string -> 'a) ->
  (Xml.ename -> Xml.attrib list -> 'a) ->
  (Xml.ename -> Xml.attrib list -> 'a list -> 'a) ->
  Xml.elt ->
  'a
```
```ocaml
val all_entities : Xml.elt -> string list
```
```ocaml
val translate : 
  (Xml.ename -> Xml.attrib list -> Xml.elt) ->
  (Xml.ename -> Xml.attrib list -> Xml.elt list -> Xml.elt) ->
  ('state -> Xml.ename -> Xml.attrib list -> Xml.elt list) ->
  ('state -> Xml.ename -> Xml.attrib list -> Xml.elt list -> Xml.elt list) ->
  (Xml.ename -> Xml.attrib list -> 'state -> 'state) ->
  'state ->
  Xml.elt ->
  Xml.elt
```