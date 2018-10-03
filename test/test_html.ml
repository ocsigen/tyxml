open Tyxml

let to_string = Format.asprintf "%a" (Html.pp_elt ())

let tyxml_tests l =
  let f (name, (ty : Html_types.body_content Html.elt), s) =
    name, `Quick, fun () -> Alcotest.(check string) name (to_string ty) s
  in
  List.map f l

let html_elements = "html elements", tyxml_tests Html.[

  "div",
  div [a []],
  "<div><a></a></div>" ;

  "a",
  canvas [a []],
  "<canvas><a></a></canvas>";

]

let escaping = "html escaping", tyxml_tests Html.[

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

  "comment",
  tot (Xml.comment
         {|[if IE 8]> <html class="no-js lt-ie9" lang="en"> <![endif]|}),
  {|<!--[if IE 8]> <html class="no-js lt-ie9" lang="en"> <![endif]-->|} ;

  "dodgy comment 1",
  tot (Xml.comment {|><script BOUM/>|}),
  {|<!--&gt;<script BOUM/>-->|} ;

  "dodgy comment 2",
  tot (Xml.comment {|-><script BOUM/>|}),
  {|<!---&gt;<script BOUM/>-->|} ;

  "dodgy comment 3",
  tot (Xml.comment {|foo--><script BOUM/>|}),
  {|<!--foo--&gt;<script BOUM/>-->|} ;

  "dodgy comment 4",
  tot (Xml.comment {|foo--!><script BOUM/>|}),
  {|<!--foo--!&gt;<script BOUM/>-->|} ;

  "utf8",
  a ~a:[a_href "/text/λαμδα"] [pcdata "λαμδα"],
  {|<a href="/text/λαμδα">λαμδα</a>|} ;

]


let tests = [
  html_elements ;
  escaping ;
]

let () = Alcotest.run "tyxml" tests
