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

]

let svg_filters = "svg filters", tyxml_tests Svg.[

  "filter gaussian blur",
  filter ~a:[ a_x (-0.1, None) ; a_y (-0.1, None) ; a_width (0.2, None) ; a_height (0.2, None) ]
    [ feGaussianBlur ~a:[a_stdDeviation (0.2, None)] [] ],
  "<filter x=\"-0.1\" y=\"-0.1\" width=\"0.2\" height=\"0.2\"><feGaussianBlur stdDeviation=\"0.2\"></feGaussianBlur></filter>" ;

  "linear gradient",
  linearGradient ~a:[ a_gradientTransform [`Rotate ((10., None), Some (0.5, 0.5))] ]
    [
      stop ~a:[ a_offset (`Percentage 0.) ; a_stop_color "white" ] [] ;
      stop ~a:[ a_offset (`Percentage 100.) ; a_stop_color "red" ] []
    ],
  "<linearGradient gradientTransform=\"rotate(10 0.5 0.5)\"><stop offset=\"0%\" stop-color=\"white\"></stop><stop offset=\"100%\" stop-color=\"red\"></stop></linearGradient>"

]

let tests = [
  svg_attributes ;
  svg_filters
]

let () = Alcotest.run "tyxml-svg" tests
