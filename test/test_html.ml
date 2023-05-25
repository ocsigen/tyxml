open Tyxml_test

let html_elements = "html elements", tyxml_tests Html.[

  "dialog",
  dialog ~a:[a_open ()] [div []],
  "<dialog open=\"open\"><div></div></dialog>" ;

  "div",
  div [a []],
  "<div><a></a></div>" ;

  "input",
  input ~a:[a_formaction "post.html"; a_formmethod `Post] (),
  "<input formaction=\"post.html\" formmethod=\"POST\"/>";

  "a",
  canvas [a []],
  "<canvas><a></a></canvas>";

  "template",
  template ~a:[a_id "idtmpl"] [p [txt "Template"]],
  "<template id=\"idtmpl\"><p>Template</p></template>" ;
  "picture_src",
  div [
    picture ~a:[a_id "idpicture"]
      ~img:(img ~a:[a_id "idimg"] ~src:"picture/img.png" ~alt:"test picture/img.png" ()) [
        source ~a:[a_mime_type "image/webp"; a_src "picture/img1.webp"] ()
      ; source ~a:[a_mime_type "image/jpeg"; a_src "picture/img2.jpg"] ()
    ]
  ],
  {|<div><picture id="idpicture">|}
    ^ {|<source type="image/webp" src="picture/img1.webp"/>|}
    ^ {|<source type="image/jpeg" src="picture/img2.jpg"/>|}
    ^ {|<img src="picture/img.png" alt="test picture/img.png" id="idimg"/>|}
    ^ {|</picture></div>|} ;

  "picture_srcset",
  div [
    picture ~a:[a_id "idpicture"]
      ~img:(img ~a:[a_id "idimg"] ~src:"picture/img.png" ~alt:"test picture/img.png" ()) [
      source ~a:[a_mime_type "image/webp";
                 a_srcset [`Url (Xml.uri_of_string "picture/img1.webp")]] ()
    ; source ~a:[a_mime_type "image/jpeg";
                 a_srcset [`Url (Xml.uri_of_string "picture/img2.jpg")]] ()
    ]
  ],
  {|<div><picture id="idpicture">|}
    ^ {|<source type="image/webp" srcset="picture/img1.webp"/>|}
    ^ {|<source type="image/jpeg" srcset="picture/img2.jpg"/>|}
    ^ {|<img src="picture/img.png" alt="test picture/img.png" id="idimg"/>|}
  ^ {|</picture></div>|} ;
]

let html_attributes = "html attributes", tyxml_tests Html.[

  "translate",
  div ~a:[a_translate `No] [p ~a:[a_translate `Yes] []],
  "<div translate=\"no\"><p translate=\"yes\"></p></div>" ;

]

let escaping = "html escaping", tyxml_tests Html.[

  "cdata",
  cdata "<bar>]]>foo<bar/>",
  "\n<![CDATA[\n<bar>foo<bar/>\n]]>\n" ;

  "cdata multi",
  cdata "<bar>]]>foo<b]]>ar/>",
  "\n<![CDATA[\n<bar>foo<bar/>\n]]>\n" ;

  "cdata_script" ,
  cdata_script "<bar>]]>foo<bar/>" ,
  "\n//<![CDATA[\n<bar>foo<bar/>\n//]]>\n" ;

  "cdata_style" ,
  cdata_style "<bar>]]>foo<bar/>" ,
  "\n/* <![CDATA[ */\n<bar>foo<bar/>\n/* ]]> */\n" ;

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
  a ~a:[a_href "/text/λαμδα"] [txt "λαμδα"],
  {|<a href="/text/λαμδα">λαμδα</a>|} ;

]


let tests = [
  html_elements ;
  html_attributes ;
  escaping ;
]

let () = Alcotest.run "tyxml" tests
