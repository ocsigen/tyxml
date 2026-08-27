open Tyxml

let tyxml_tests l =
  let f (name, ty, s) =
    let to_string = Format.asprintf "%a" (Svg.pp_elt ()) in
    name, `Quick, fun () -> Alcotest.(check string) name (to_string ty) s
  in
  List.map f l

let svg_attributes = "svg attributes", tyxml_tests Svg.[

  "text data-foo",
  text ~a:[ a_user_data "foo" "valfoo" ] [],
  "<text data-foo=\"valfoo\"></text>" ;

  "symbol id",
  symbol ~a:[ a_id "sym" ] [],
  "<symbol id=\"sym\"></symbol>" ;

  "g xml:lang",
  g ~a:[ a_xml_lang "fr" ] [],
  "<g xml:lang=\"fr\"></g>" ;

  "rect ontouchstart",
  rect ~a:[ a_ontouchstart "f(event)" ] [],
  "<rect ontouchstart=\"f(event)\"></rect>" ;

]

let svg_filters = "svg filters", tyxml_tests Svg.[

  "filter gaussian blur",
  filter ~a:[ a_x (-0.1, None) ; a_y (-0.1, None) ; a_width (0.2, None) ; a_height (0.2, None) ]
    [ feGaussianBlur ~a:[a_stdDeviation (0.2, None)] [] ],
  "<filter x=\"-0.1\" y=\"-0.1\" width=\"0.2\" height=\"0.2\"><feGaussianBlur stdDeviation=\"0.2\"></feGaussianBlur></filter>" ;

  "feMerge",
  filter [ feMerge [ feMergeNode ~a:[ a_in (`Ref "a") ] [] ;
                     feMergeNode ~a:[ a_in `SourceGraphic ] [] ] ],
  {|<filter><feMerge><feMergeNode in="a"></feMergeNode><feMergeNode in="SourceGraphic"></feMergeNode></feMerge></filter>|} ;

  "linear gradient",
  linearGradient ~a:[ a_gradientTransform [`Rotate ((10., None), Some (0.5, 0.5))] ]
    [
      stop ~a:[ a_offset (`Percentage 0.) ; a_stop_color "white" ] [] ;
      stop ~a:[ a_offset (`Percentage 100.) ; a_stop_color "red" ] []
    ],
  "<linearGradient gradientTransform=\"rotate(10 0.5 0.5)\"><stop offset=\"0%\" stop-color=\"white\"></stop><stop offset=\"100%\" stop-color=\"red\"></stop></linearGradient>"

]

let svg_mask = "svg mask", tyxml_tests Svg.[

  "mask",
  mask ~a:[ a_id "m" ; a_maskUnits `UserSpaceOnUse ]
    [ rect ~a:[ a_width (10., None) ; a_height (10., None) ] [] ],
  {|<mask id="m" maskUnits="userSpaceOnUse"><rect width="10" height="10"></rect></mask>|} ;

]

let svg_clip_path = "svg clip-path", tyxml_tests Svg.[

  "use with clip-path",
  use ~a:[ a_clip_path "url(#test-clip)"; a_href "#test-object"] [],
  {|<use clip-path="url(#test-clip)" href="#test-object"></use>|}

]

let tests = [
  svg_attributes ;
  svg_filters ;
  svg_mask ;
  svg_clip_path
]

let () = Alcotest.run "tyxml-svg" tests
