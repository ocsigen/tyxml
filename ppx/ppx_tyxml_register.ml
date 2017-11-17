open Migrate_parsetree

let () =
  Driver.register
    ~name:"tyxml" Versions.ocaml_403
    Ppx_tyxml.mapper
