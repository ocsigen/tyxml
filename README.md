# TyXML

TyXML is a library to build statically correct Html5 and Svg documents.

```ocaml
let to_ocaml = Html5.(a ~a:[a_href "ocaml.org"] [pcdata "OCaml!"])
```

TyXML provides a set of combinators to build Html5 and Svg documents. These combinators use the OCaml type-system to ensure the validity of the generated Html5 and Svg.
TyXML's combinators are used in various libraries, such as [Eliom][] and [Js_of_ocaml][], but a builtin implementation is also provided by the `tyxml` ocamlfind package.

The documentation can be consulted [on the TyXML website](https://ocsigen.org/tyxml/manual/). Examples are available in the [examples](examples) directory.

[Eliom]: https://ocsigen.org/eliom/manual/clientserver-html
[Js_of_ocaml]: https://ocsigen.org/js_of_ocaml/api/Tyxml_js

## How to

### Installation

TyXML is available in [opam](https://opam.ocaml.org/):
```sh
opam install tyxml
```

To use the development version, pin it:
```sh
opam pin add tyxml --dev
```

### Manual build

For manual builds, please consult [the included opam file](opam).
