open Tyxml_test

let html_elements = "html elements", tyxml_tests Html.[

  "dialog",
  dialog ~a:[a_open ()] [div []],
  "<dialog open=\"open\"><div></div></dialog>" ;

  "s",
  p [s [txt "old price"]],
  "<p><s>old price</s></p>" ;

  "bdi",
  p [bdi [txt "user123"]],
  "<p><bdi>user123</bdi></p>" ;

  "search",
  search [p [txt "results"]],
  "<search><p>results</p></search>" ;

  "data",
  p [data ~value:"42" [txt "forty-two"]],
  "<p><data value=\"42\">forty-two</data></p>" ;

  "slot",
  div [slot ~a:[a_name "icon"] [txt "fallback"]],
  "<div><slot name=\"icon\">fallback</slot></div>" ;

  "track",
  video ~tracks:[
    track ~src:"video_en.vtt"
      ~a:[a_kind `Subtitles; a_track_srclang "en";
          a_label "English"; a_default ()] ()
  ] [],
  "<video><track src=\"video_en.vtt\" kind=\"subtitles\" srclang=\"en\""
  ^ " label=\"English\" default=\"default\"/></video>" ;

  "div",
  div [a []],
  "<div><a></a></div>" ;

  "form control attributes",
  div [
    input ~a:[a_input_type `File; a_capture `Environment] () ;
    input ~a:[a_input_type `Text; a_name "comment";
              a_dirname "comment.dir"] () ;
    textarea ~a:[a_dirname "t.dir"; a_autocomplete `Off] (txt "") ;
    select ~a:[a_autocomplete `Off] []
  ],
  "<div><input type=\"file\" capture=\"environment\"/>"
  ^ "<input type=\"text\" name=\"comment\" dirname=\"comment.dir\"/>"
  ^ "<textarea dirname=\"t.dir\" autocomplete=\"off\"></textarea>"
  ^ "<select autocomplete=\"off\"></select></div>" ;

  "a ping",
  p [a ~a:[a_href "/x"; a_ping ["https://t.example/ping"];
           a_referrerpolicy `No_referrer] [txt "x"]],
  "<p><a href=\"/x\" ping=\"https://t.example/ping\""
  ^ " referrerpolicy=\"no-referrer\">x</a></p>" ;

  "script nomodule",
  script ~a:[a_nomodule (); a_blocking [`Render]] (txt ""),
  "<script nomodule=\"nomodule\" blocking=\"render\"></script>" ;

  "img loading",
  img ~src:"x.png" ~alt:"x"
    ~a:[a_loading `Lazy; a_decoding `Async; a_fetchpriority `Low] (),
  "<img src=\"x.png\" alt=\"x\" loading=\"lazy\" decoding=\"async\""
  ^ " fetchpriority=\"low\"/>" ;

  "iframe srcdoc",
  iframe ~a:[a_srcdoc "<p>Hi</p>"; a_allow "fullscreen"; a_loading `Lazy] [],
  "<iframe srcdoc=\"&lt;p&gt;Hi&lt;/p&gt;\" allow=\"fullscreen\""
  ^ " loading=\"lazy\"></iframe>" ;

  "popovertarget",
  button ~a:[a_popovertarget "pop"; a_popovertargetaction `Toggle]
    [txt "Toggle"],
  "<button popovertarget=\"pop\""
  ^ " popovertargetaction=\"toggle\">Toggle</button>" ;

  "invoker commands",
  div [
    button ~a:[a_commandfor "dlg"; a_command `Show_modal] [txt "Open"] ;
    button ~a:[a_commandfor "dlg"; a_command (`Other "--my-cmd")]
      [txt "Custom"]
  ],
  "<div><button commandfor=\"dlg\" command=\"show-modal\">Open</button>"
  ^ "<button commandfor=\"dlg\" command=\"--my-cmd\">Custom</button></div>" ;

  "shadow parts",
  div ~a:[a_part ["label"; "value"];
          a_exportparts ["inner-label"; "inner-value:value"]] [],
  "<div part=\"label value\""
  ^ " exportparts=\"inner-label, inner-value:value\"></div>" ;

  "microdata",
  div ~a:[a_itemscope (); a_itemtype ["https://schema.org/Person"];
          a_itemid "urn:isbn:123"; a_itemref ["a"; "b"]]
    [span ~a:[a_itemprop ["name"]] [txt "X"]],
  "<div itemscope=\"itemscope\" itemtype=\"https://schema.org/Person\""
  ^ " itemid=\"urn:isbn:123\" itemref=\"a b\">"
  ^ "<span itemprop=\"name\">X</span></div>" ;

  "global attributes",
  div ~a:[a_popover `Auto; a_inert (); a_dir `Auto;
          a_autocapitalize `Words; a_autocorrect true;
          a_writingsuggestions false; a_enterkeyhint `Go;
          a_nonce "n0nce"; a_slot "myslot"; a_is "word-count"] [],
  "<div popover=\"auto\" inert=\"inert\" dir=\"auto\""
  ^ " autocapitalize=\"words\" autocorrect=\"on\""
  ^ " writingsuggestions=\"false\" enterkeyhint=\"go\""
  ^ " nonce=\"n0nce\" slot=\"myslot\" is=\"word-count\"></div>" ;

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
