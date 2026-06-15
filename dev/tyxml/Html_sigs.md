
# Module `Html_sigs`

HTML signatures for the functorial interface.

```ocaml
module type T = sig ... end
```
Signature of typesafe constructors for HTML documents.

```ocaml
module type NoWrap = T with module Xml.W = Xml_wrap.NoWrap
```
Equivalent to [`T`](./Html_sigs-module-type-T.md), but without wrapping.


### Signature functors

` See {{!page-"functors"}the manual of the functorial interface}. `

```ocaml
module Make
  (Xml : Xml_sigs.T)
  (Svg : Svg_sigs.T with module Xml := Xml) : 
  sig ... end
```
Signature functor for [`Html_f.Make`](./Html_f-Make.md).

```ocaml
module type Wrapped_functions = sig ... end
```
Wrapped functions, to be used with [`Html_f.Make_with_wrapped_functions`](./Html_f-Make_with_wrapped_functions.md).
