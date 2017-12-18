(* TyXML
 * http://www.ocsigen.org/tyxml
 * Module Myocamlbuild
 * Copyright (C) 2010 Jérémie Dimino
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
*)
[@@@ocaml.warning "-3"]
(* OASIS_START *)
(* OASIS_STOP *)
# 26 "myocamlbuild.ml"

open Ocamlbuild_plugin

(* Determine extension of CompiledObject: best *)
let native_suffix =
  let env = BaseEnvLight.load () in
  if BaseEnvLight.var_get "is_native" env = "true"
  then "native" else "byte"

let reflect_ppx () =
  let ppx_reflect = "ppx/ppx_reflect."^native_suffix in

  let prod = "ppx/%_reflected.ml" in
  let dep = "lib/%.mli" in

  rule "ppx_reflect: mli -> _reflected.ml" ~prod ~deps:[dep; ppx_reflect]
    begin fun env _ ->
      Cmd (S [A ppx_reflect ; P (env dep); P (env prod)])
    end

let tyxml_ppx () =
  let ppx_tyxml = "ppx/ppx_tyxml_standalone."^native_suffix in
  flag_and_dep [ "ocaml" ; "compile" ; "ppx_tyxml" ]
    (S [A "-ppx"; Quote (S [P ppx_tyxml; Sh "--as-ppx"])])

let () =
  dispatch
    (fun hook ->
       dispatch_default hook;
       match hook with
       | After_rules ->

         (* Api: use an introduction page with categories *)
         tag_file "tyxml-api.docdir/index.html" ["apiref"];
         dep ["apiref"] ["doc/indexdoc"];
         flag ["apiref"] & S[A "-intro"; P "doc/indexdoc";
                             A"-colorize-code" ; A"-short-functors" ;
                             A"-charset"; P "utf-8" ];

         (* the "-bin-annot" flag was introduced in ocaml-4.00 *)
         (* the "bin_annot" tag was only introduced in ocamlbuild-4.01 *)
         if String.sub Sys.ocaml_version 0 4 = "4.00" then
           flag ["ocaml"; "bin_annot"; "compile"] (A "-bin-annot");

         reflect_ppx () ;
         tyxml_ppx () ;

       | _ ->
         ())

(* Rules for testing non-typable things *)
open Ocamlbuild_pack
let () =
  let toplevel env _build =
    let arg = env "%.top.ml" and out = env "%.result" in
    let tags = tags_of_pathname arg in
    Cmd(S[Sh "TERM=dumb ocaml -noinit -noprompt";
          T tags ; Sh " < " ; P arg;
          Sh " 2>&1 | tail -n +19 >"; P out ])
  in
  Rule.rule
    "toplevel: %.top.ml -> %.result"
      ~dep:"%.top.ml"
      ~prod:"%.result"
      toplevel

let () =
  let diff env _build =
    let res = env "%.result" and exp = env "%.expected" in
    Cmd(S[A "diff"; P res ; P exp ])
  in
  Rule.rule
    "diff: %.result + %.expected -> %.stamp"
      ~stamp:"%.stamp"
      ~deps:["%.result"; "%.expected"]
      diff

(* Compile the wiki version of the Ocamldoc.

   Thanks to Till Varoquaux on usenet:
   http://www.digipedia.pl/usenet/thread/14273/231/

*)

let ocamldoc_wiki tags deps docout docdir =
  let tags = tags -- "extension:html" in
  Ocamlbuild_pack.Ocaml_tools.ocamldoc_l_dir tags deps docout docdir

let () =
  try
    let wikidoc_dir =
      let base = Ocamlbuild_pack.My_unix.run_and_read "ocamlfind query wikidoc" in
      String.sub base 0 (String.length base - 1)
    in

    Ocamlbuild_pack.Rule.rule
      "ocamldoc: document ocaml project odocl & *odoc -> wikidocdir"
        ~insert:`top
        ~prod:"%.wikidocdir/index.wiki"
        ~stamp:"%.wikidocdir/wiki.stamp"
        ~dep:"%.odocl"
        (Ocamlbuild_pack.Ocaml_tools.document_ocaml_project
           ~ocamldoc:ocamldoc_wiki
           "%.odocl" "%.wikidocdir/index.wiki" "%.wikidocdir");

      tag_file "tyxml-api.wikidocdir/index.wiki" ["apiref";"wikidoc"];
      flag ["wikidoc"] & S[A"-i";A wikidoc_dir;A"-g";A"odoc_wiki.cma"]

  with Failure e -> () (* Silently fail if the package wikidoc isn't available *)
