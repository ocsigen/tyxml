(lang dune 2.7)
(name tyxml)

(generate_opam_files true)

(maintainers "Ocsigen team <dev@ocsigen.org>")
(authors "Ocsigen team <dev@ocsigen.org>")
(license "LGPL-2.1-only with OCaml-LGPL-linking-exception")
(source (github ocsigen/tyxml))
(documentation "https://ocsigen.org/tyxml/latest/manual/intro")

(package
 (name tyxml-jsx)
 (synopsis "JSX syntax to write TyXML documents")
 (description
"```reason
open Tyxml;
let to_reason = <a href=\"reasonml.github.io/\"> \"Reason!\" </a>
```

The TyXML JSX allow to write TyXML documents with reason's JSX syntax.
It works with textual trees, virtual DOM trees, or any TyXML module.
")
 (depends
  (ocaml (>= 4.04))
  (tyxml (= :version))
  (tyxml-syntax (= :version))
  (ppxlib (>= 0.36))
  (alcotest :with-test)
  (reason :with-test)))

(package
 (name tyxml-ppx)
 (synopsis "PPX to write TyXML documents with the HTML syntax")
 (description
"```ocaml
open Tyxml
let%html to_ocaml = \"<a href='ocaml.org'>OCaml!</a>\"
```

The TyXML PPX allow to write TyXML documents using the traditional HTML syntax.
It works with textual trees, virtual DOM trees, or any TyXML module.
")
 (depends
  (ocaml (>= 4.04))
  (tyxml (= :version))
  (tyxml-syntax (= :version))
  (markup (>= 0.7.2))
  (ppxlib (>= 0.36))
  (re (>= 1.5.0))
  (alcotest :with-test)))

(package
 (name tyxml-syntax)
 (synopsis "Common layer for the JSX and PPX syntaxes for Tyxml")
 (depends
  (ocaml (>= 4.03))
  (ppxlib (>= 0.36))
  (re (>= 1.5.0))
  (uutf (>= 1.0.0))
  (alcotest :with-test)))

(package
 (name tyxml)
 (synopsis "A library for building correct HTML and SVG documents")
 (description "TyXML provides a set of convenient combinators that uses the OCaml type system to ensure the validity of the generated documents. TyXML can be used with any representation of HTML and SVG: the textual one, provided directly by this package, or DOM trees (`js_of_ocaml-tyxml`) virtual DOM (`virtual-dom`) and reactive or replicated trees (`eliom`). You can also create your own representation and use it to instantiate a new set of combinators.")
 (depends
  (ocaml (>= 4.03))
  (re (>= 1.5.0))
  (uutf (>= 1.0.0))
  seq
  (alcotest :with-test)))
