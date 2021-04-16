(lang dune 2.0)

(name tyxml)

(formatting
 (enabled_for dune ocaml))

(license "LGPL-2.1 with OCaml linking exception")

(authors "The ocsigen team")

(maintainers "dev@ocsigen.org")

(source
 (github ocsigen/tyxml))

(bug_reports "https://github.com/ocsigen/tyxml/issues")

(homepage "https://github.com/ocsigen/tyxml")

(documentation "https://ocsigen.org/tyxml/latest/manual/intro")

(generate_opam_files true)

(package
 (name tyxml-jsx)
 (synopsis "JSX syntax to write TyXML documents")
 (description
  "Allow to write any TyXML documents with reason's JSX syntax, from textual trees to reactive virtual DOM trees.")
 (depends
  (ocaml
   (>= 4.02))
  (tyxml :version)
  (tyxml-syntax :version)
  (alcotest :with-test)
  (reason :with-test)
  ppx_tools_versioned))

(package
 (name tyxml-ppx)
 (synopsis "PPX that allows to write TyXML documents with the HTML syntax")
 (description
  "The TyXML PPX is compatible with all TyXML instance, from textual trees to reactive virtual DOM trees.")
 (depends
  (ocaml
   (>= 4.02))
  (tyxml :version)
  (tyxml-syntax :version)
  (alcotest :with-test)
  (markup
   (>= 0.7.2))
  ppx_tools_versioned))

(package
 (name tyxml-syntax)
 (synopsis "Common layer for the JSX and PPX syntaxes for Tyxml")
 (depends
  (ocaml
   (>= 4.02))
  (tyxml :version)
  (tyxml-syntax :version)
  (alcotest :with-test)
  ppx_tools_versioned
  (re
   (>= 1.5.0))
  (uutf
   (>= 1.0.0))))

(package
 (name tyxml)
 (synopsis "TyXML is a library for building correct HTML and SVG documents")
 (description
  "TyXML provides a set of convenient combinators that uses the OCaml type system to ensure the validity of the generated documents. TyXML can be used with any representation of HTML and SVG: the textual one, provided directly by this package, or DOM trees (`js_of_ocaml-tyxml`) virtual DOM (`virtual-dom`) and reactive or replicated trees (`eliom`). You can also create your own representation and use it to instantiate a new set of combinators.")
 (depends
  (ocaml
   (>= 4.02))
  (alcotest :with-test)
  (re
   (>= 1.5.0))
  seq
  (uutf
   (>= 1.0.0))))
