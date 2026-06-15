
# Module `Tyxml_syntax`

```ocaml
module Attribute_value : sig ... end
```
Attribute value parsers and parser combinators.

```ocaml
module Attributes : sig ... end
```
Attribute parsing.

```ocaml
module Common : sig ... end
```
```ocaml
module Element : sig ... end
```
Element parsing.

```ocaml
module Element_content : sig ... end
```
Element child argument assemblers. These are almost parsers, except they only tell how to pass already-parsed children to element functions.

```ocaml
module Html_sigs_reflected : sig ... end
```
```ocaml
module Html_types_reflected : sig ... end
```
```ocaml
module Name_convention : sig ... end
```
Gives the tyxml names for HTML elements and attributes.

```ocaml
module Namespace : sig ... end
```
Namespace-specific values.

```ocaml
module Sigs_reflected : sig ... end
```
Signature of `Html_sigs_reflected` and `Svg_sigs_reflected` (but not `Html_types_reflected`).

```ocaml
module Svg_sigs_reflected : sig ... end
```
```ocaml
module Svg_types_reflected : sig ... end
```