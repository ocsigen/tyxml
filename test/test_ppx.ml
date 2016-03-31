(** Ppx Tests

    This file is here to torture the ppx. Tests that are directly related to
    html or svg should go to the other files.
*)

open Html5

module TyTests = struct
  type t = Xml.elt list
  let pp fmt x =
    P.print_list ~output:(Format.pp_print_string fmt) (M.totl x)
  let equal = (=)
end


let tyxml_tests l =
  let f (name, ty1, ty2) =
    name, `Quick, fun () ->
      Alcotest.(check (module TyTests)) name (M.toeltl ty1) (M.toeltl ty2)
  in
  List.map f l

module Html5 = M
let basics = "ppx basics", tyxml_tests M.[

  "elems",
  [%tyxml "<p></p>"],
  [p []] ;

  "child",
  [%tyxml "<p><span>foo</span></p>"],
  [p [span [pcdata "foo"]]] ;

  "list",
  [%tyxml "<p></p><span>foo</span>"],
  [p [] ; span [pcdata "foo"]] ;

  "attrib",
  [%tyxml "<p id=foo></p>"],
  [p ~a:[a_id "foo"] []] ;

  "attribs",
  [%tyxml "<p id=foo class=bar></p>"],
  [p ~a:[a_id "foo"; a_class ["bar"] ] []] ;

  "comment",
  [%tyxml "<!--foo-->"],
  [tot @@ Xml.comment "foo"]

]

let elt1 = M.(span [pcdata "one"])
let elt2 = M.(b [pcdata "two"])
let id = "pata"

let antiquot = "ppx antiquot", tyxml_tests M.[

  "child",
  [%tyxml "<p>" elt1 "</p>"],
  [p [elt1]];

  "children",
  [%tyxml "<p>bar"elt1"foo"elt2"baz</p>"],
  [p [pcdata "bar"; elt1 ; pcdata "foo" ; elt2 ; pcdata "baz" ]];

  "insertion",
  [%tyxml "<p><em>" elt1 "</em></p>"],
  [p [em [elt1]]];

  "attrib",
  [%tyxml "<p id="id">bla</p>"],
  [p ~a:[a_id id] [pcdata "bla"]];

  (* should succeed *)
  (* "escape", *)
  (* [%tyxml "<p>(tyxml4)</p>"], *)
  (* [p [pcdata "(tyxml4)"]]; *)


]



let tests = [
  basics ;
  antiquot ;
]
