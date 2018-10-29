# TyXML

TyXML is a library for building statically correct HTML5 and SVG documents:

```ocaml
open Tyxml
let to_ocaml = Html.(a ~a:[a_href "ocaml.org"] [txt "OCaml!"])
```

Tyxml can also be used with the standard HTML syntax, using the PPX:

```ocaml
open Tyxml
let%html to_ocaml = "<a href='ocaml.org'>OCaml!</a>"
```

TyXML provides a set of combinators. These combinators use the OCaml type system
to ensure the validity of the generated document.
They are used in various libraries, such as [Eliom][] and [Js_of_ocaml][].

The documentation can be consulted
[on the TyXML website](https://ocsigen.org/tyxml/manual/). Examples are
available in the [examples](examples) directory.

[Eliom]: https://ocsigen.org/eliom/manual/clientserver-html
[Js_of_ocaml]: https://ocsigen.org/js_of_ocaml/api/Tyxml_js

## Installation

TyXML is available in [OPAM](https://opam.ocaml.org/):
```sh
opam install tyxml
```

To install the PPX:
```sh
opam install tyxml-ppx
```
