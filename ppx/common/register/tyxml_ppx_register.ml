open Migrate_parsetree

let () =
  Driver.register
    ~name:"tyxml" Versions.ocaml_405
    Tyxml_ppx.mapper
