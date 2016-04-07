(** Ppx Tests

    This file is here to torture the ppx. Tests that are directly related to
    html or svg should go to the other files.
*)

open Tyxml

module TyTests = struct
  type t = Xml.elt list
  let pp fmt x =
    Format.pp_print_list ~pp_sep:(fun _ () -> ())
      (Html5.pp_elt ())
      fmt (Html5.totl x)
  let equal = (=)
end


let tyxml_tests l =
  let f (name, ty1, ty2) =
    name, `Quick, fun () ->
      Alcotest.(check (module TyTests)) name (Html5.toeltl ty1) (Html5.toeltl ty2)
  in
  List.map f l

let basics = "ppx basics", tyxml_tests Html5.[

  "elems",
  [[%html5 "<p></p>"]],
  [p []] ;

  "child",
  [[%html5 "<p><span>foo</span></p>"]],
  [p [span [pcdata "foo"]]] ;

  "list",
  [%html5 "<p></p><span>foo</span>"],
  [p [] ; span [pcdata "foo"]] ;

  "attrib",
  [[%html5 "<p id=foo></p>"]],
  [p ~a:[a_id "foo"] []] ;

  "attribs",
  [[%html5 "<p id=foo class=bar></p>"]],
  [p ~a:[a_id "foo"; a_class ["bar"] ] []] ;

  "comment",
  [[%html5 "<!--foo-->"]],
  [tot @@ Xml.comment "foo"] ;

  "pcdata",
  [[%html5 "foo"]],
  [pcdata "foo"] ;


]

let ns_nesting = "namespace nesting" , tyxml_tests Html5.[

  "html/svg",
  [[%html5 "<svg><g></g></svg>"]],
  [svg [Svg.g []]] ;

  "nested svg",
  [[%html5 "<div><svg><g></g></svg></div>"]],
  [div [svg [Svg.g []]]] ;

  "with_neighbour",
  [[%html5 "<div><span></span><svg><g></g></svg>foo</div>"]],
  [div [span [] ; svg [Svg.g []] ; pcdata "foo" ]] ;

  "ambiguous tag",
  [[%html5 "<svg><a></a></svg>"]],
  [svg [Svg.a []]] ;

]

let elt1 = Html5.(span [pcdata "one"])
let elt2 = Html5.[b [pcdata "two"]]
let id = "pata"

let antiquot = "ppx antiquot", tyxml_tests Html5.[

  "child",
  [[%html5 "<p>" [elt1] "</p>"]],
  [p [elt1]];

  "list child",
  [[%html5 "<p>" elt2 "</p>"]],
  [p elt2];

  "children",
  [[%html5 "<p>bar"[elt1]"foo"elt2"baz</p>"]],
  [p ([pcdata "bar"; elt1 ; pcdata "foo" ] @ elt2 @ [pcdata "baz" ])];

  "insertion",
  [[%html5 "<p><em>"[elt1]"</em></p>"]],
  [p [em [elt1]]];

  "attrib",
  [[%html5 "<p id="id">bla</p>"]],
  [p ~a:[a_id id] [pcdata "bla"]];

  (* should succeed *)
  (* "escape", *)
  (* [%tyxml "<p>(tyxml4)</p>"], *)
  (* [p [pcdata "(tyxml4)"]]; *)


]



let tests = [
  basics ;
  ns_nesting ;
  antiquot ;
]
