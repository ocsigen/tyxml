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

  "animate begin, dur, end",
  animate ~a:[ a_begin "0s" ; a_dur "2s" ; a_end "4s" ] [],
  {|<animate begin="0s" dur="2s" end="4s"></animate>|} ;

  "fePointLight x, y, z",
  fePointLight ~a:[ a_x (1., None) ; a_y (2., None) ; a_z 3. ] [],
  {|<fePointLight x="1" y="2" z="3"></fePointLight>|} ;

  "filter filterUnits",
  filter ~a:[ a_filterUnits `ObjectBoundingBox ] [],
  {|<filter filterUnits="objectBoundingBox"></filter>|} ;

  "style title",
  style ~a:[ a_title "alternate" ] (txt ""),
  {|<style title="alternate"></style>|} ;

  "symbol geometry",
  symbol ~a:[ a_x (0., None) ; a_y (0., None) ; a_width (10., None) ;
              a_height (10., None) ; a_refX (5., None) ; a_refY (5., None) ]
    [],
  {|<symbol x="0" y="0" width="10" height="10" refX="5" refY="5"></symbol>|} ;

  "tabindex and lang",
  circle ~a:[ a_tabindex 0 ; a_lang "fr" ] [],
  {|<circle tabindex="0" lang="fr"></circle>|} ;

  "role and aria-*",
  g ~a:[ a_role ["img"] ; a_aria "label" ["a circle"] ] [],
  {|<g role="img" aria-label="a circle"></g>|} ;

  "textPath path and side",
  textPath ~a:[ a_path "M 0 0 L 10 10" ; a_side `Right ] [],
  {|<textPath path="M 0 0 L 10 10" side="right"></textPath>|} ;

  "image crossorigin, decoding, fetchpriority",
  image ~a:[ a_href "i.png" ; a_crossorigin `Anonymous ; a_decoding `Async ;
             a_fetchpriority `High ] [],
  {|<image href="i.png" crossorigin="anonymous" decoding="async" fetchpriority="high"></image>|} ;

]

let svg_links = "svg links", tyxml_tests Svg.[

  "a with SVG 2 link attributes",
  a ~a:[ a_href "page.html" ; a_target "_blank" ; a_rel ["noopener"] ;
         a_hreflang "fr" ; a_type "text/html" ; a_ping ["/p1"; "/p2"] ;
         a_referrerpolicy `No_referrer ; a_download (Some "page.html") ]
    [],
  {|<a href="page.html" target="_blank" rel="noopener" hreflang="fr" type="text/html" ping="/p1 /p2" referrerpolicy="no-referrer" download="page.html"></a>|} ;

  "a with a nameless download",
  a ~a:[ a_href "f.png" ; a_download None ] [],
  {|<a href="f.png" download=""></a>|} ;

]

let svg_presentation = "svg presentation attributes", tyxml_tests Svg.[

  "opacity, fill-opacity, stroke-opacity",
  rect ~a:[ a_opacity 0.5 ; a_fill_opacity 0.25 ; a_stroke_opacity 1. ] [],
  {|<rect opacity="0.5" fill-opacity="0.25" stroke-opacity="1"></rect>|} ;

  "display, visibility, overflow",
  g ~a:[ a_display "none" ; a_visibility `Collapse ; a_overflow `Hidden ] [],
  {|<g display="none" visibility="collapse" overflow="hidden"></g>|} ;

  "pointer-events",
  circle ~a:[ a_pointer_events `VisiblePainted ] [],
  {|<circle pointer-events="visiblePainted"></circle>|} ;

  "clip-rule, shape-rendering",
  path ~a:[ a_clip_rule `Evenodd ; a_shape_rendering `CrispEdges ] [],
  {|<path clip-rule="evenodd" shape-rendering="crispEdges"></path>|} ;

  "color-interpolation-filters",
  filter ~a:[ a_color_interpolation_filters `LinearRGB ] [],
  {|<filter color-interpolation-filters="linearRGB"></filter>|} ;

  "markers",
  path ~a:[ a_marker_start "url(#m1)" ; a_marker_mid "url(#m2)" ;
            a_marker_end "url(#m3)" ] [],
  {|<path marker-start="url(#m1)" marker-mid="url(#m2)" marker-end="url(#m3)"></path>|} ;

  "mask and filter references",
  g ~a:[ a_mask "url(#m)" ; a_filter "url(#f)" ] [],
  {|<g mask="url(#m)" filter="url(#f)"></g>|} ;

  "flood-color, flood-opacity, lighting-color",
  feFlood ~a:[ a_flood_color "red" ; a_flood_opacity 0.5 ;
               a_lighting_color "white" ] [],
  {|<feFlood flood-color="red" flood-opacity="0.5" lighting-color="white"></feFlood>|} ;

  "text presentation",
  text ~a:[ a_letter_spacing "0.1em" ; a_word_spacing "normal" ;
            a_direction `Rtl ; a_unicode_bidi `Bidi_override ;
            a_writing_mode `Vertical_rl ] [],
  {|<text letter-spacing="0.1em" word-spacing="normal" direction="rtl" unicode-bidi="bidi-override" writing-mode="vertical-rl"></text>|} ;

  "color, baseline-shift, font-size-adjust",
  text ~a:[ a_color "blue" ; a_baseline_shift "super" ;
            a_font_size_adjust "0.5" ] [],
  {|<text color="blue" baseline-shift="super" font-size-adjust="0.5"></text>|} ;

  "cursor, image-rendering, color-rendering",
  image ~a:[ a_cursor "pointer" ; a_image_rendering `OptimizeQuality ;
             a_color_rendering `OptimizeSpeed ] [],
  {|<image cursor="pointer" image-rendering="optimizeQuality" color-rendering="optimizeSpeed"></image>|} ;

  "color-interpolation",
  g ~a:[ a_color_interpolation `SRGB ] [],
  {|<g color-interpolation="sRGB"></g>|} ;

  "SVG 2 values of existing attributes",
  path ~a:[ a_stroke_linejoin `Miter_clip ;
            a_alignment_baseline `Text_top ;
            a_dominant_baseline `Text_bottom ] [],
  {|<path stroke-linejoin="miter-clip" alignment-baseline="text-top" dominant-baseline="text-bottom"></path>|} ;

  "feBlend CSS blend mode",
  feBlend ~a:[ a_mode `Color_dodge ] [],
  {|<feBlend mode="color-dodge"></feBlend>|} ;

  "paint-order, vector-effect, transform-origin",
  path ~a:[ a_paint_order "stroke fill" ;
            a_vector_effect `Non_scaling_stroke ;
            a_transform_origin "center" ] [],
  {|<path paint-order="stroke fill" vector-effect="non-scaling-stroke" transform-origin="center"></path>|} ;

  "white-space, text-overflow",
  text ~a:[ a_white_space `Pre_wrap ; a_text_overflow `Ellipsis ] [],
  {|<text white-space="pre-wrap" text-overflow="ellipsis"></text>|} ;

]

let svg_filters = "svg filters", tyxml_tests Svg.[

  "filter gaussian blur",
  filter ~a:[ a_x (-0.1, None) ; a_y (-0.1, None) ; a_width (0.2, None) ; a_height (0.2, None) ]
    [ feGaussianBlur ~a:[a_stdDeviation (0.2, None)] [] ],
  "<filter x=\"-0.1\" y=\"-0.1\" width=\"0.2\" height=\"0.2\"><feGaussianBlur stdDeviation=\"0.2\"></feGaussianBlur></filter>" ;

  "feDropShadow",
  filter [ feDropShadow ~a:[ a_dx 2. ; a_dy 2. ; a_stdDeviation (1., None) ;
                             a_flood_color "black" ; a_flood_opacity 0.5 ] [] ],
  {|<filter><feDropShadow dx="2" dy="2" stdDeviation="1" flood-color="black" flood-opacity="0.5"></feDropShadow></filter>|} ;

  "feMerge",
  filter [ feMerge [ feMergeNode ~a:[ a_in (`Ref "a") ] [] ;
                     feMergeNode ~a:[ a_in `SourceGraphic ] [] ] ],
  {|<filter><feMerge><feMergeNode in="a"></feMergeNode><feMergeNode in="SourceGraphic"></feMergeNode></feMerge></filter>|} ;

  "radial gradient fr",
  radialGradient ~a:[ a_r (0.5, None) ; a_fr (0.1, None) ] [],
  {|<radialGradient r="0.5" fr="0.1"></radialGradient>|} ;

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
  svg_links ;
  svg_presentation ;
  svg_filters ;
  svg_mask ;
  svg_clip_path
]

let () = Alcotest.run "tyxml-svg" tests
