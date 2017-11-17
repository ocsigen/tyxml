open Migrate_parsetree

let () =
  Driver.register
    ~name:"tyxml" Versions.ocaml_405
    Ppx_tyxml.mapper
