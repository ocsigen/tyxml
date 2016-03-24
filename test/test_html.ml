
let to_string = Format.asprintf "%a" (Html5.pp_elt ())

let tyxml_tests l =
  let f (name, (ty : Html5_types.body_content Html5.elt), s) =
    name, `Quick, fun () -> Alcotest.(check string) name (to_string ty) s
  in
  List.map f l

let html_elements = "html elements", tyxml_tests Html5.[

  "div",
  div [a []],
  "<div><a></a></div>" ;

  "a",
  canvas [a []],
  "<canvas><a></a></canvas>";

]

let escaping = "html escaping", tyxml_tests Html5.[

  "cdata",
  cdata "<bar>]]>foo<bar/>",
  {|
<![CDATA[
<bar>foo<bar/>
]]>
|} ;

  "cdata multi",
  cdata "<bar>]]>foo<b]]>ar/>",
  {|
<![CDATA[
<bar>foo<bar/>
]]>
|} ;

  "cdata_script" ,
  cdata_script "<bar>]]>foo<bar/>" ,
  {|
//<![CDATA[
<bar>foo<bar/>
//]]>
|} ;

  "cdata_style" ,
  cdata_style "<bar>]]>foo<bar/>" ,
  {|
/* <![CDATA[ */
<bar>foo<bar/>
/* ]]> */
|} ;

]


let tests = [
  html_elements ;
  escaping ;
]
