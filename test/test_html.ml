open Tyxml_test

let html_elements = "html elements", tyxml_tests Html.[

  "dialog",
  dialog ~a:[a_open ()] [div []],
  "<dialog open=\"open\"><div></div></dialog>" ;

  "s",
  p [s [txt "old price"]],
  "<p><s>old price</s></p>" ;

  "form method dialog",
  form ~a:[a_method `Dialog] [],
  "<form method=\"dialog\"></form>" ;

  "iframe sandbox tokens",
  iframe ~a:[a_sandbox [`Allow_downloads; `Allow_modals;
                        `Allow_popups_to_escape_sandbox]] [],
  "<iframe sandbox=\"allow-downloads allow-modals"
  ^ " allow-popups-to-escape-sandbox\"></iframe>" ;

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

  "global event handlers",
  div ~a:[a_onbeforetoggle "b()"; a_ontoggle "t()"; a_oncopy "c()";
          a_onpaste "p()"; a_onscrollend "s()"] [],
  "<div onbeforetoggle=\"b()\" ontoggle=\"t()\" oncopy=\"c()\""
  ^ " onpaste=\"p()\" onscrollend=\"s()\"></div>" ;

  "pointer events",
  div ~a:[a_onpointerdown "d()"; a_onwheel "w()"; a_onauxclick "a()"] [],
  "<div onpointerdown=\"d()\" onwheel=\"w()\" onauxclick=\"a()\"></div>" ;

  "declarative shadow DOM",
  div [template ~a:[a_shadowrootmode `Open; a_shadowrootdelegatesfocus ();
                    a_shadowrootclonable (); a_shadowrootserializable ()]
         [p [txt "shadow"]]],
  "<div><template shadowrootmode=\"open\""
  ^ " shadowrootdelegatesfocus=\"shadowrootdelegatesfocus\""
  ^ " shadowrootclonable=\"shadowrootclonable\""
  ^ " shadowrootserializable=\"shadowrootserializable\">"
  ^ "<p>shadow</p></template></div>" ;

  "video attributes",
  video ~a:[a_playsinline (); a_disablepictureinpicture ();
            a_disableremoteplayback ()] [],
  "<video playsinline=\"playsinline\""
  ^ " disablepictureinpicture=\"disablepictureinpicture\""
  ^ " disableremoteplayback=\"disableremoteplayback\"></video>" ;

  "ol type",
  ol ~a:[a_ol_type `Upper_roman; a_start 3] [li [txt "x"]],
  "<ol type=\"I\" start=\"3\"><li>x</li></ol>" ;

  "th abbr",
  tablex [tbody [tr [th ~a:[a_abbr "Pop."] [txt "Population"]]]],
  "<table><tbody><tr><th abbr=\"Pop.\">Population</th></tr></tbody></table>" ;

  "dialog closedby",
  dialog ~a:[a_closedby `Closerequest] [div []],
  "<dialog closedby=\"closerequest\"><div></div></dialog>" ;

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

let html_content_models = "html content models", tyxml_tests Html.[

  "dl with div groups",
  dl [ div [ dt [ txt "term" ] ; dd [ txt "definition" ] ] ],
  {|<dl><div><dt>term</dt><dd>definition</dd></div></dl>|} ;

  "lists with script-supporting elements",
  div [ ol [ li [ txt "1" ] ; script (txt "f()") ] ;
        ul [ li [ txt "a" ] ; template [ p [ txt "b" ] ] ] ],
  {|<div><ol><li>1</li><script>f()</script></ol>|}
  ^ {|<ul><li>a</li><template><p>b</p></template></ul></div>|} ;

  "tables with script-supporting elements",
  tablex ~thead:(thead [ tr [ th [ txt "h" ] ; script (txt "f()") ] ])
    [ tbody [ tr [ td [ txt "c" ] ] ; script (txt "g()") ] ],
  {|<table><thead><tr><th>h</th><script>f()</script></tr></thead>|}
  ^ {|<tbody><tr><td>c</td></tr><script>g()</script></tbody></table>|} ;

  "select with script-supporting elements",
  select [ optgroup ~label:"g" [ option (txt "o") ; script (txt "f()") ] ;
           script (txt "g()") ],
  {|<select><optgroup label="g"><option>o</option><script>f()</script>|}
  ^ {|</optgroup><script>g()</script></select>|} ;

  "menu with li and script-supporting elements",
  menu ~children:(`Lis [ li [ txt "1" ] ; script (txt "f()") ]) (),
  {|<menu><li>1</li><script>f()</script></menu>|} ;

  "meta itemprop in flow content",
  p [meta_itemprop ~itemprop:["name"] ~a:[a_content "Ada"] ()],
  "<p><meta itemprop=\"name\" content=\"Ada\"/></p>" ;

  "details name",
  details ~a:[a_name "accordion"; a_open ()]
    (summary [txt "s"]) [txt "d"],
  "<details name=\"accordion\" open=\"open\">"
  ^ "<summary>s</summary>d</details>" ;

  "image map with areas",
  p [ map ~a:[a_name "m"]
        [ area ~alt:"home" ~a:[a_href (uri_of_string "/");
                               a_shape `Rect; a_coords [0; 0; 10; 10];
                               a_referrerpolicy `No_referrer] () ] ],
  {|<p><map name="m"><area alt="home" href="/" shape="rect"|}
  ^ {| coords="0,0,10,10" referrerpolicy="no-referrer"/></map></p>|} ;

  "hr separators in select",
  select [ option (txt "a") ; hr () ; option (txt "b") ],
  {|<select><option>a</option><hr/><option>b</option></select>|} ;

  "hgroup with p elements",
  hgroup [ h1 [ txt "Standard" ] ; p [ txt "Last updated" ] ],
  {|<hgroup><h1>Standard</h1><p>Last updated</p></hgroup>|} ;

  "dl with script-supporting elements",
  dl [ dt [ txt "t" ] ; dd [ txt "d" ] ; script (txt "f()") ],
  {|<dl><dt>t</dt><dd>d</dd><script>f()</script></dl>|} ;

]

let html_attributes = "html attributes", tyxml_tests Html.[

  "translate",
  div ~a:[a_translate `No] [p ~a:[a_translate `Yes] []],
  "<div translate=\"no\"><p translate=\"yes\"></p></div>" ;

  "hidden",
  div [div ~a:[a_hidden `Hidden] []; div ~a:[a_hidden `Until_found] []],
  "<div><div hidden=\"hidden\"></div>"
  ^ "<div hidden=\"until-found\"></div></div>" ;

  (* [a_srclang] is a deprecated alias of [a_xml_lang]; the [srclang] attribute
     of [track] is [a_track_srclang], tested above. *)
  "srclang",
  div ~a:[(a_srclang [@alert "-deprecated"]) "fr"] [],
  "<div xml:lang=\"fr\"></div>" ;

  "contenteditable",
  div [div ~a:[a_contenteditable `True] [];
       div ~a:[a_contenteditable `False] [];
       div ~a:[a_contenteditable `Plaintext_only] []],
  "<div><div contenteditable=\"true\"></div>"
  ^ "<div contenteditable=\"false\"></div>"
  ^ "<div contenteditable=\"plaintext-only\"></div></div>" ;

  (* The escape hatches of Unsafe cover the URI list attributes too. *)
  "unsafe uris",
  div ~a:[Unsafe.uris_attrib "data-urls" ["/a"; "/b"]] [],
  {|<div data-urls="/a /b"></div>|} ;

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


let printing = "printing", [
  "xml declaration", `Quick, (fun () ->
    Alcotest.(check string) "xml declaration"
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      (Xml_print.compose_decl ())) ;

  "xml declaration with an encoding", `Quick, (fun () ->
    Alcotest.(check string) "xml declaration with an encoding"
      "<?xml version=\"1.1\" encoding=\"US-ASCII\"?>\n"
      (Xml_print.compose_decl ~version:"1.1" ~encoding:"US-ASCII" ())) ;

  (* The document printer emits the doctype of the language. *)
  "doctype", `Quick, (fun () ->
    Alcotest.(check string) "doctype"
      "<!DOCTYPE html>\n<html xmlns=\"http://www.w3.org/1999/xhtml\">\
       <head><title>t</title></head><body></body></html>"
      (Format.asprintf "%a" (Html.pp ())
         Html.(html (head (title (txt "t")) []) (body [])))) ;
]

let tests = [
  html_elements ;
  html_content_models ;
  html_attributes ;
  escaping ;
  printing ;
]

let () = Alcotest.run "tyxml" tests
