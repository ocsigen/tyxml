(** Ppx Tests

    This file is here to torture the ppx. Tests that are directly related to
    html or svg should go to the other files.
*)

open Tyxml

module type LANGUAGE = sig
  include Xml_sigs.Typed_pp
  type 'a list_wrap = 'a Xml_wrap.NoWrap.tlist
  val totl : Xml.elt list_wrap -> ('a elt) list_wrap
  val toeltl : ('a elt) list_wrap -> Xml.elt list_wrap
end

module TyTests (Language : LANGUAGE) = struct
  module Testable = struct
    type t = Xml.elt list
    let pp fmt x =
      Format.pp_print_list ~pp_sep:(fun _ () -> ())
        (Language.pp_elt ())
        fmt (Language.totl x)
    let equal = (=)
  end

  let make l =
    let f (name, ty1, ty2) =
      name, `Quick, fun () ->
        Alcotest.(check (module Testable)) name
          (Language.toeltl ty1) (Language.toeltl ty2)
    in
    List.map f l
end

module HtmlTests = TyTests (Html)
module SvgTests = TyTests (Svg)

let basics = "ppx basics", HtmlTests.make Html.[

  "elems",
  [[%html "<p></p>"]],
  [p []] ;

  "child",
  [[%html "<p><span>foo</span></p>"]],
  [p [span [pcdata "foo"]]] ;

  "list",
  [%html "<p></p><span>foo</span>"],
  [p [] ; span [pcdata "foo"]] ;

  "attrib",
  [[%html "<p id=foo></p>"]],
  [p ~a:[a_id "foo"] []] ;

  "attribs",
  [[%html "<p id=foo class=bar></p>"]],
  [p ~a:[a_id "foo"; a_class ["bar"] ] []] ;

  "comment",
  [[%html "<!--foo-->"]],
  [tot @@ Xml.comment "foo"] ;

  "pcdata",
  [[%html "foo"]],
  [pcdata "foo"] ;

  "document",
  [[%html "<html><head><title>foo</title></head></html>"]],
  [html (head (title (pcdata "foo")) []) (body [])] ;

  "let",
  [let%html x = "<p></p>" in x],
  [p []] ;

  "nested let",
  [let%html _ = "<p></p>" in
   let%html y = "<p></p>" in
   y],
  [p []] ;

  "let and",
  (let%html x = "<p></p>" and y = "<a></a>" in [x;y]),
  [p []; a []] ;

  "let fun",
  [let%html f x = "<p>"x"</p>" in f [a []]],
  [p [a []]] ;

  "whitespace in html element",
  [[%html "<html><head><title>foo</title></head> </html>"]],
  [html (head (title (pcdata "foo")) []) (body [])] ;

  "whitespace around html element",
  [[%html "  <html><head><title>foo</title></head></html>  "]],
  [html (head (title (pcdata "foo")) []) (body [])] ;

  "whitespace around element",
  [[%html "   <p></p>   "]],
  [p []] ;

  "whitespace in element",
  [[%html "   <p>  </p>   "]],
  [p [pcdata "  "]] ;

  "whitespace around lists",
  [%html "   <p></p><span></span>   "],
  [p [] ; span []] ;

  "whitespace around pcdata",
  [%html "   bar<p></p>foo   "],
  [pcdata "   bar" ; p [] ; pcdata "foo   " ] ;

  "whitespace in ul",
  [[%html "<ul>   <li>foo</li>  <li>bar</li>   </ul>"]],
  [ul [li [pcdata "foo"] ; li [pcdata "bar"]]] ;

  "whitespace in ol",
  [[%html "<ol>   <li>foo</li>  <li>bar</li>   </ol>"]],
  [ol [li [pcdata "foo"] ; li [pcdata "bar"]]] ;

]

let attribs = "ppx attribs", HtmlTests.make Html.[

  "unit absent",
  [[%html "<div hidden></div>"]],
  [div ~a:[a_hidden ()] []] ;

  "unit present",
  [[%html "<div hidden=hidden></div>"]],
  [div ~a:[a_hidden ()] []] ;

  "bool default",
  [[%html "<div draggable></div>"]],
  [div ~a:[a_draggable true] []] ;

  "bool true",
  [[%html "<div draggable=true></div>"]],
  [div ~a:[a_draggable true] []] ;

  "bool false",
  [[%html "<div draggable=false></div>"]],
  [div ~a:[a_draggable false] []] ;

  "onoff default",
  [[%html "<form autocomplete></form>"]],
  [form ~a:[a_autocomplete true] []] ;

  "bool true",
  [[%html "<form autocomplete=on></form>"]],
  [form ~a:[a_autocomplete true] []] ;

  "bool false",
  [[%html "<form autocomplete=off></form>"]],
  [form ~a:[a_autocomplete false] []] ;

  "link rel=canonical",
  [[%html "<link rel=canonical href='/'>"]],
  [link ~rel:[`Canonical] ~href:"/" ()] ;

  "embed type",
  [[%html "<embed type='text/plain'>"]],
  [embed ~a:[a_mime_type "text/plain"] ()] ;

  "output for",
  [[%html "<output for=foo></output>"]],
  [output_elt ~a:[a_output_for ["foo"]] []] ;

  "input min time",
  [[%html "<input min='2002-10-02T15:00:00Z'>"]],
  [input ~a:[a_input_min (`Datetime "2002-10-02T15:00:00Z")] ()] ;

]

let ns_nesting = "namespace nesting" , HtmlTests.make Html.[

  "html/svg",
  [[%html "<svg><g></g></svg>"]],
  [svg [Svg.g []]] ;

  "nested svg",
  [[%html "<div><svg><g></g></svg></div>"]],
  [div [svg [Svg.g []]]] ;

  "with_neighbour",
  [[%html "<div><span></span><svg><g></g></svg>foo</div>"]],
  [div [span [] ; svg [Svg.g []] ; pcdata "foo" ]] ;

  "ambiguous tag",
  [[%html "<svg><a></a></svg>"]],
  [svg [Svg.a []]] ;

]

let elt1 = Html.(span [pcdata "one"])
let elt2 = Html.[b [pcdata "two"]]
let id = "pata"

let antiquot = "ppx antiquot", HtmlTests.make Html.[

  "child",
  [[%html "<p>" [elt1] "</p>"]],
  [p [elt1]];

  "list child",
  [[%html "<p>" elt2 "</p>"]],
  [p elt2];

  "children",
  [[%html "<p>bar"[elt1]"foo"elt2"baz</p>"]],
  [p ([pcdata "bar"; elt1 ; pcdata "foo" ] @ elt2 @ [pcdata "baz" ])];

  "insertion",
  [[%html "<p><em>"[elt1]"</em></p>"]],
  [p [em [elt1]]];

  "attrib",
  [[%html "<p id="id">bla</p>"]],
  [p ~a:[a_id id] [pcdata "bla"]];

  "first child",
  [%html [elt1] "<p></p>"],
  [elt1 ; p []];

  "last child",
  [%html "<p></p>" [elt1] ],
  [p []; elt1];

  (* should succeed *)
  (* "escape", *)
  (* [%tyxml "<p>(tyxml4)</p>"], *)
  (* [p [pcdata "(tyxml4)"]]; *)


]

let svg = "svg", SvgTests.make Svg.[

  "basic",
  [[%svg "<svg/>"]],
  [svg []] ;

  "transform",
  [[%svg "<line transform='translate(1) translate(2)'/>"]],
  [line ~a:[a_transform [`Translate (1., None); `Translate (2., None)]] []] ;

  "offset percentage",
  [[%svg "<stop offset='50.1%'/>"]],
  [stop ~a:[a_offset (`Percentage 50.1)] []] ;

  "text x, y",
  [[%svg "<text x='1 2' y='3 4'/>"]],
  [text ~a:[a_x_list [1., None; 2., None]; a_y_list [3., None; 4., None]] []] ;

  "text dx, dy",
  [[%svg "<text dx='1 2' dy='3 4'/>"]],
  [text
    ~a:[a_dx_list [1., None; 2., None]; a_dy_list [3., None; 4., None]] []] ;

  "feColorMatrix type",
  [[%svg "<feColorMatrix type='matrix'/>"]],
  [feColorMatrix ~a:[a_feColorMatrix_type `Matrix] []] ;

  "feTurbulence type",
  [[%svg "<feTurbulence type='fractalNoise'/>"]],
  [feTurbulence ~a:[a_feTurbulence_type `FractalNoise] []] ;

  "animateTransform type",
  [[%svg "<animateTransform type='translate'/>"]],
  [animateTransform ~a:[a_animateTransform_type `Translate] []] ;

  "feFuncR type, offset",
  [[%svg "<feFuncR type='identity' offset='0'/>"]],
  [feFuncR ~a:[a_transfer_type `Identity; a_transfer_offset 0.] []] ;

  "feComposite operator",
  [[%svg "<feComposite operator='xor'/>"]],
  [feComposite ~a:[a_feComposite_operator `Xor] []] ;

  "feMorphology operator",
  [[%svg "<feMorphology operator='erode'/>"]],
  [feMorphology ~a:[a_feMorphology_operator `Erode] []] ;

  "animation fill, values",
  [[%svg "<animation fill='freeze' values='1 2'/>"]],
  [animation ~a:[a_animation_fill `Freeze; a_animation_values ["1"; "2"]] []] ;

]

let svg_element_names = "svg element names", SvgTests.make Svg.[

  "textPath", [[%svg "<textPath/>"]], [textPath []] ;
  "linearGradient", [[%svg "<linearGradient/>"]], [linearGradient []] ;
  "radialGradient", [[%svg "<radialGradient/>"]], [radialGradient []] ;
  "clipPath", [[%svg "<clipPath/>"]], [clipPath []] ;
  "feDistantLight", [[%svg "<feDistantLight/>"]], [feDistantLight []] ;
  "fePointLight", [[%svg "<fePointLight/>"]], [fePointLight []] ;
  "feSpotLight", [[%svg "<feSpotLight/>"]], [feSpotLight []] ;
  "feBlend", [[%svg "<feBlend/>"]], [feBlend []] ;
  "feColorMatrix", [[%svg "<feColorMatrix/>"]], [feColorMatrix []] ;
  "feComponentTransfer",
  [[%svg "<feComponentTransfer/>"]], [feComponentTransfer []] ;
  "feFuncA", [[%svg "<feFuncA/>"]], [feFuncA []] ;
  "feFuncG", [[%svg "<feFuncG/>"]], [feFuncG []] ;
  "feFuncB", [[%svg "<feFuncB/>"]], [feFuncB []] ;
  "feFuncR", [[%svg "<feFuncR/>"]], [feFuncR []] ;
  "feComposite", [[%svg "<feComposite/>"]], [feComposite []] ;
  "feConvolveMatrix", [[%svg "<feConvolveMatrix/>"]], [feConvolveMatrix []] ;
  "feDiffuseLighting", [[%svg "<feDiffuseLighting/>"]], [feDiffuseLighting []] ;
  "feDisplacementMap", [[%svg "<feDisplacementMap/>"]], [feDisplacementMap []] ;
  "feFlood", [[%svg "<feFlood/>"]], [feFlood []] ;
  "feGaussianBlur", [[%svg "<feGaussianBlur/>"]], [feGaussianBlur []] ;
  "feImage", [[%svg "<feImage/>"]], [feImage []] ;
  "feMerge", [[%svg "<feMerge/>"]], [feMerge []] ;
  "feMorphology", [[%svg "<feMorphology/>"]], [feMorphology []] ;
  "feOffset", [[%svg "<feOffset/>"]], [feOffset []] ;
  "feSpecularLighting",
  [[%svg "<feSpecularLighting/>"]], [feSpecularLighting []] ;
  "feTile", [[%svg "<feTile/>"]], [feTile []] ;
  "feTurbulence", [[%svg "<feTurbulence/>"]], [feTurbulence []] ;
  "animateMotion", [[%svg "<animateMotion/>"]], [animateMotion []] ;
  "animateColor", [[%svg "<animateColor/>"]], [animateColor []] ;
  "animateTransform", [[%svg "<animateTransform/>"]], [animateTransform []] ;

]



let tests = [
  basics ;
  attribs ;
  ns_nesting ;
  antiquot ;
  svg ;
  svg_element_names ;
]
