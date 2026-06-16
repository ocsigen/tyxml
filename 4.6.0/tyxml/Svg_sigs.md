
# Module `Svg_sigs`

SVG signatures for the functorial interface.

```ocaml
module type T = sig ... end
```
Signature of typesafe constructors for SVG documents.

```ocaml
module type NoWrap = T with module Xml.W = Xml_wrap.NoWrap
```
Equivalent to [`T`](./Svg_sigs-module-type-T.md), but without wrapping.


### Signature functors

See [the manual of the functorial interface](./functors.md).

```ocaml
module Make (Xml : Xml_sigs.T) : sig ... end
```
Signature functor for [`Svg_f.Make`](./Svg_f-Make.md).

```ocaml
module type Wrapped_functions = sig ... end
```
Wrapped functions, to be used with [`Svg_f.Make_with_wrapped_functions`](./Svg_f-Make_with_wrapped_functions.md).
