
# Module `Svg_f`

Typesafe constructors for SVG documents (Functorial interface)

This module is experimental, it may lack of some attributes, and the interface is very low level and do not take deeply into account the needs of SVG elements.

` See {{!page-"functors"}the manual of the functorial interface}. `

```ocaml
module Make
  (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b) : 
  Svg_sigs.Make(Xml).T
    with type +'a elt = Xml.elt
     and type +'a attrib = Xml.attrib
```
Create a new implementation of `Svg`, using the given underlying `Xml` implementation. Will output a module of type [`Svg_sigs.T`](./Svg_sigs-module-type-T.md) with the various type equalities.

```ocaml
module Wrapped_functions
  (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b) : 
  Svg_sigs.Wrapped_functions with module Xml = Xml
```
The standard set of wrapped functions, when `W.ft` is the regular function.

```ocaml
module Make_with_wrapped_functions
  (Xml : Xml_sigs.T)
  (C : Svg_sigs.Wrapped_functions with module Xml = Xml) : 
  Svg_sigs.Make(Xml).T
    with type +'a elt = Xml.elt
     and type +'a attrib = Xml.attrib
```
Similar to [`Make`](./Svg_f-Make.md) but with a custom set of wrapped functions.
