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

(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin

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

       | _ ->
         ())

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
