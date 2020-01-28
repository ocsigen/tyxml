open Migrate_parsetree

let () =
  Driver.register
    ~name:"tyxml-jsx" Versions.ocaml_405
    Tyxml_jsx.mapper
