
# Module `Html_f`

Typesafe constructors for HTML documents (Functorial interface)

See [the manual of the functorial interface](./functors.md).

```ocaml
module Make
  (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
  (Svg : Svg_sigs.T with module Xml := Xml) : 
  Html_sigs.Make(Xml)(Svg).T
    with type +'a elt = Xml.elt
     and type +'a attrib = Xml.attrib
```
Create a new implementation of `HTML`, using the given underlying `Xml` and `Svg` implementation. Will output a module of type [`Html_sigs.T`](./Html_sigs-module-type-T.md) with the various type equalities.

```ocaml
module Wrapped_functions
  (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b) : 
  Html_sigs.Wrapped_functions with module Xml = Xml
```
The standard set of wrapped functions, when `W.ft` is the regular function.

```ocaml
module Make_with_wrapped_functions
  (Xml : Xml_sigs.T)
  (C : Html_sigs.Wrapped_functions with module Xml = Xml)
  (Svg : Svg_sigs.T with module Xml := Xml) : 
  Html_sigs.Make(Xml)(Svg).T
    with type +'a elt = Xml.elt
     and type +'a attrib = Xml.attrib
```
Similar to [`Make`](./Html_f-Make.md) but with a custom set of wrapped functions.
