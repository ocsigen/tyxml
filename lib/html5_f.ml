(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2004 by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
 * Copyright (C) 2007 by Vincent Balat, Gabriel Kerneis
 * Copyright (C) 2010 by Cecile Herbelin
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Suite 500, Boston, MA 02111-1307, USA.
 *)

(* TODO :
   - MathML and SVG
   - forbid construction like that noscript (a [a []])
   by playing on interactive_without*
*)

(* IDEAS:
     The [a_] prefix would have to be maintained and the
     only advantage are a potentially better mapping of the XHTML modularization
     to O'Caml modules. *)

open Html5_types

module MakeWrapped
    (W : Xml_wrap.T)
    (Xml : Xml_sigs.Wrapped with type 'a wrap = 'a W.t)
    (Svg : Svg_sigs.T with module Xml := Xml)= struct

  module Xml = Xml

  module Info = struct
    let content_type = "text/html"
    let alternative_content_types = ["application/xhtml+xml";"application/xml";"text/xml"]
    let version = "HTML5-draft"
    let standard = "http://www.w3.org/TR/html5/"
    let namespace = "http://www.w3.org/1999/xhtml"
    let doctype =
      Xml_print.compose_doctype "html" []
    let emptytags =
      [ "area"; "base"; "br"; "col"; "command"; "embed"; "hr"; "img";
        "input"; "keygen"; "link"; "meta"; "param"; "source"; "wbr" ]
  end

  type 'a wrap = 'a W.t

  let opt_fmap f x def = match x with
    | None -> W.return def
    | Some x -> W.fmap f x
  let opt_w x def = match x with
    | None -> W.return def
    | Some x -> x


  type uri = Xml.uri
  let string_of_uri = Xml.string_of_uri
  let uri_of_string = Xml.uri_of_string

  type 'a attrib = Xml.attrib

  let to_xmlattribs x = x
  let to_attrib x = x

  (* VB *)
  let float_attrib = Xml.float_attrib

  let int_attrib = Xml.int_attrib

  let string_attrib = Xml.string_attrib

  let uri_attrib a s = Xml.uri_attrib a s

  let space_sep_attrib = Xml.space_sep_attrib

  let comma_sep_attrib = Xml.comma_sep_attrib

  let user_attrib f name v = Xml.string_attrib name (W.fmap f v)

  let event_handler_attrib = Xml.event_handler_attrib

  (* Deprecated alias. *)
  let event_attrib = Xml.event_handler_attrib

  (* space-separated *)
  let length_to_string = function
    | `Pixels p -> string_of_int p
    | `Percent p -> (string_of_int p) ^ "%"

  let length_attrib name x =
    user_attrib length_to_string name x

  let multilength_to_string = function
    | `Pixels p -> string_of_int p
    | `Percent p -> (string_of_int p) ^ "%"
    | `Relative 1 -> "*"
    | `Relative i -> (string_of_int i) ^ "*"

  let multilength_attrib name x =
    user_attrib multilength_to_string name x

  let multilength_to_string m =
    String.concat ", " (List.map multilength_to_string m)

  let multilengths_attrib name x =
    user_attrib multilength_to_string name x

  let linktype_to_string =
    function
    | `Alternate -> "alternate"
    | `Archives -> "archives"
    | `Author -> "author"
    | `Bookmark -> "bookmark"
    | `External -> "external"
    | `First -> "first"
    | `Help -> "help"
    | `Icon -> "icon"
    | `Index -> "index"
    | `Last -> "last"
    | `License -> "license"
    | `Next -> "next"
    | `Nofollow -> "nofollow"
    | `Noreferrer -> "noreferrer"
    | `Pingback -> "pingback"
    | `Prefetch -> "prefetch"
    | `Prev -> "prev"
    | `Search -> "search"
    | `Stylesheet -> "stylesheet"
    | `Sidebar -> "sidebar"
    | `Tag -> "tag"
    | `Up -> "up"
    | `Other t -> t

  let linktypes_to_string l =
    String.concat " " (List.map linktype_to_string l)

  let linktypes_attrib name linktypes =
    user_attrib linktypes_to_string name linktypes

  let mediadesc_to_string =
    function
    | `All -> "all"
    | `Aural -> "aural"
    | `Braille -> "braille"
    | `Embossed -> "embossed"
    | `Handheld -> "handheld"
    | `Print -> "print"
    | `Projection -> "projection"
    | `Screen -> "screen"
    | `Speech -> "speech"
    | `TTY -> "tty"
    | `TV -> "tv"
    | `Raw_mediadesc s -> s

  let mediadescs_to_string mediadescs =
    String.concat ", " (List.map mediadesc_to_string mediadescs)

  let mediadesc_attrib name mediadescs =
    user_attrib mediadescs_to_string name mediadescs

  (* Core: *)
  let a_class = space_sep_attrib "class"

  let a_id = string_attrib "id"

  let a_user_data name = string_attrib ("data-" ^ name)

  let a_title = string_attrib "title"

  (* I18N: *)
  let a_xml_lang = string_attrib "xml:lang"

  (* Style: *)
  let a_style = string_attrib "style"

  let a_property = string_attrib "property"

  (* Events: *)
  let a_onabort = event_attrib "onabort"

  let a_onafterprint = event_attrib "onafterprint"

  let a_onbeforeprint = event_attrib "onbeforeprint"

  let a_onbeforeunload = event_attrib "onbeforeunload"

  let a_onblur = event_attrib "onblur"

  let a_oncanplay = event_attrib "oncanplay"

  let a_oncanplaythrough = event_attrib "oncanplaythrough"

  let a_onchange = event_attrib "onchange"

  let a_onclick = event_attrib "onclick"

  let a_oncontextmenu = event_attrib "oncontextmenu"

  let a_ondblclick = event_attrib "ondblclick"

  let a_ondrag = event_attrib "ondrag"

  let a_ondragend = event_attrib "ondragend"

  let a_ondragenter = event_attrib "ondragenter"

  let a_ondragleave = event_attrib "ondragleave"

  let a_ondragover = event_attrib "ondragover"

  let a_ondragstart = event_attrib "ondragstart"

  let a_ondrop = event_attrib "ondrop"

  let a_ondurationchange = event_attrib "ondurationchange"

  let a_onemptied = event_attrib "onemptied"

  let a_onended = event_attrib "onended"

  let a_onerror = event_attrib "onerror"

  let a_onfocus = event_attrib "onfocus"

  let a_onformchange = event_attrib "onformchange"

  let a_onforminput = event_attrib "onforminput"

  let a_onhashchange = event_attrib "onhashchange"

  let a_oninput = event_attrib "oninput"

  let a_oninvalid = event_attrib "oninvalid"

  let a_onmousedown = event_attrib "onmousedown"

  let a_onmouseup = event_attrib "onmouseup"

  let a_onmouseover = event_attrib "onmouseover"

  let a_onmousemove = event_attrib "onmousemove"

  let a_onmouseout = event_attrib "onmouseout"

  let a_onmousewheel = event_attrib "onmousewheel"

  let a_onoffline = event_attrib "onoffline"

  let a_ononline = event_attrib "ononline"

  let a_onpause = event_attrib "onpause"

  let a_onplay = event_attrib "onplay"

  let a_onplaying = event_attrib "onplaying"

  let a_onpagehide = event_attrib "onpagehide"

  let a_onpageshow = event_attrib "onpageshow"

  let a_onpopstate = event_attrib "onpopstate"

  let a_onprogress = event_attrib "onprogress"

  let a_onratechange = event_attrib "onratechange"

  let a_onreadystatechange = event_attrib "onreadystatechange"

  let a_onredo = event_attrib "onredo"

  let a_onresize = event_attrib "onresize"

  let a_onscroll = event_attrib "onscroll"

  let a_onseeked = event_attrib "onseeked"

  let a_onseeking = event_attrib "onseeking"

  let a_onselect = event_attrib "onselect"

  let a_onshow = event_attrib "onshow"

  let a_onstalled = event_attrib "onstalled"

  let a_onstorage = event_attrib "onstorage"

  let a_onsubmit = event_attrib "onsubmit"

  let a_onsuspend = event_attrib "onsuspend"

  let a_ontimeupdate = event_attrib "ontimeupdate"

  let a_onundo = event_attrib "onundo"

  let a_onunload = event_attrib "onunload"

  let a_onvolumechange = event_attrib "onvolumechange"

  let a_onwaiting = event_attrib "onwaiting"

  let a_onkeypress = event_attrib "onkeypress"

  let a_onkeydown = event_attrib "onkeydown"

  let a_onkeyup = event_attrib "onkeyup"

  let a_onload = event_attrib "onload"

  let a_onloadeddata = event_attrib "onloadeddata"

  let a_onloadedmetadata = event_attrib ""

  let a_onloadstart = event_attrib "onloadstart"

  let a_onmessage = event_attrib "onmessage"

    (* Other Attributes *)
  let a_version = string_attrib "version"

  let a_xmlns x =
    let f = function
      | `W3_org_1999_xhtml -> "http://www.w3.org/1999/xhtml"
    in user_attrib f "xmlns" x

  let a_manifest = uri_attrib "manifest"

  let a_cite = uri_attrib "cite"

  let a_xml_space x =
    let f = function
      | `Preserve -> "preserve"
    in user_attrib f "xml:space" x

  let a_accesskey c =
    user_attrib (String.make 1) "accesskey" c

  let a_charset = string_attrib "charset"

  let a_accept_charset = space_sep_attrib "accept-charset"

  let a_accept = space_sep_attrib "accept"

  let a_href = uri_attrib "href"

  let a_hreflang = string_attrib "hreflang"

  let a_rel = linktypes_attrib "rel"

  let a_tabindex = int_attrib "tabindex"

  let a_mime_type = string_attrib "type"

  let a_alt = string_attrib "alt"

  let a_height p = int_attrib "height" p

  let a_src = uri_attrib "src"

  let a_width p = int_attrib "width" p

  let a_for = string_attrib "for"

  let a_for_list = space_sep_attrib "for"

  let a_selected x =
    let f = function | `Selected -> "selected"
    in user_attrib f "selected" x

  let a_text_value = string_attrib "value"

  let a_int_value = int_attrib "value"

  let a_value = string_attrib "value"

  let a_float_value = float_attrib "value"

  let a_action = uri_attrib "action"

  let a_method m =
    let f = function
      | `Get -> "GET"
      | `Post -> "POST"
      | `Put -> "PUT"
      | `Delete -> "DELETE"
    in user_attrib f "method" m

  let a_enctype = string_attrib "enctype"

  let a_checked x =
    let f = function
      | `Checked -> "checked"
    in user_attrib f "checked" x

  let a_disabled x =
    let f = function
      | `Disabled -> "disabled"
    in user_attrib f "disabled" x

  let a_readonly x =
    let f = function
      | `ReadOnly -> "readonly"
    in user_attrib f "readonly" x

  let a_maxlength = int_attrib "maxlength"

  let a_name = string_attrib "name"

  let a_autocomplete ac =
    let f = function
      | `On -> "on"
      | `Off -> "off"
    in user_attrib f "autocomplete" ac

  let a_async x =
    let f = function
      | `Async -> "async"
    in user_attrib f "async" x

  let a_autofocus x =
    let f = function
      | `Autofocus -> "autofocus"
    in user_attrib f "autofocus" x

  let a_autoplay x =
    let f = function
      | `Autoplay -> "autoplay"
    in user_attrib f "autoplay" x

  let a_challenge = string_attrib "challenge"

  let a_contenteditable ce =
    let f = function
      | `True -> "true"
      | `False -> "false"
    in user_attrib f "contenteditable" ce

  let a_contextmenu = string_attrib "contextmenu"

  let a_controls x =
    let f = function
      | `Controls -> "controls"
    in user_attrib f "controls" x

  let a_dir d =
    let f = function
      | `Ltr -> "ltr"
      | `Rtl -> "rtl"
    in user_attrib f "dir" d

  let a_draggable d =
    let f = function
      | `True -> "true"
      | `False -> "false"
    in user_attrib f "draggable" d

  let a_form = string_attrib "form"

  let a_formaction = uri_attrib "formaction"

  let a_formenctype = string_attrib "formenctype"

  let a_formmethod m =
    let f = function
      | `Get -> "GET"
      | `Post -> "POST"
      | `Put -> "PUT"
      | `Delete -> "DELETE"
    in user_attrib f "method" m

  let a_formnovalidate x =
    let f = function
      | `Formnovalidate -> "formnovalidate"
    in user_attrib f "formnovalidate" x

  let a_formtarget = string_attrib "formtarget"

  let a_hidden x =
    let f = function
      | `Hidden -> "hidden"
    in user_attrib f "hidden" x

  let a_high = float_attrib "high"

  let a_icon = uri_attrib "icon"

  let a_ismap x =
    let f = function
      | `Ismap -> "ismap"
    in user_attrib f "ismap" x

  let a_keytype = string_attrib "keytype"

  let a_list = string_attrib "list"

  let a_loop x =
    let f = function
      | `Loop -> "loop"
    in user_attrib f "loop" x

  let a_low = float_attrib "low"

  let a_max = float_attrib "max"

  let a_input_max = float_attrib "max"

  let a_min = float_attrib "min"

  let a_input_min = float_attrib "min"

  let a_novalidate x =
    let f = function
      | `Novalidate -> "novalidate"
    in user_attrib f "novalidate" x

  let a_open x =
    let f = function
      | `Open -> "open"
    in user_attrib f "open" x

  let a_optimum = float_attrib "optimum"

  let a_pattern = string_attrib "pattern"

  let a_placeholder = string_attrib "placeholder"

  let a_poster = uri_attrib "poster"

  let a_preload pl =
    let f = function
      | `None -> "none"
      | `Metadata -> "metadata"
      | `Audio -> "audio"
    in user_attrib f "preload" pl

  let a_pubdate x =
    let f = function
      | `Pubdate -> "pubdate"
    in user_attrib f "pubdate" x

  let a_radiogroup = string_attrib "radiogroup"

  let a_required x =
    let f = function
      | `Required -> "required"
    in user_attrib f "required" x

  let a_reversed x =
    let f = function
      | `Reversed -> "reserved"
    in user_attrib f "reserved" x

  let rec a_sandbox sb =
    let rec aux sb =
      match sb with
        | `AllowSameOrigin :: a -> "allow-same-origin" :: (aux a)
        | `AllowForms :: a -> "allow-forms" :: (aux a)
        | `AllowScript :: a -> "allow-script" :: (aux a)
        | [] -> []
    in space_sep_attrib "sandbox" (W.fmap aux sb)

  let a_spellcheck sc =
    let f = function
      | `True -> "true"
      | `False -> "false"
    in user_attrib f "spellckeck" sc

  let a_scoped x =
    let f = function
      | `Scoped -> "scoped"
    in user_attrib f "scoped" x

  let a_seamless x =
    let f = function
      | `Seamless -> "seamless"
    in user_attrib f "seamless" x

  let a_sizes sizes =
    let f sizes = String.concat " " (List.map string_of_int sizes)
    in user_attrib f "sizes" sizes

  let a_span = int_attrib "span"

  (*let a_srcdoc*)
  let a_srclang = string_attrib "xml:lang"

  let a_start = int_attrib "start"

  let a_step step =
    let f = function
      | None -> "any"
      | Some f -> string_of_float f
    in user_attrib f "step" step

  let a_wrap w =
    let f = function
      | `Soft -> "soft"
      | `Hard -> "hard"
    in user_attrib f "wrap" w

  let a_size = int_attrib "size"

  let a_input_type it =
    let f = function
      | `Url -> "url"
      | `Tel -> "tel"
      | `Text -> "text"
      | `Time -> "time"
      | `Search -> "search"
      | `Password -> "password"
      | `Checkbox -> "checkbox"
      | `Range -> "range"
      | `Radio -> "radio"
      | `Submit -> "submit"
      | `Reset -> "reset"
      | `Number -> "number"
      | `Month -> "month"
      | `Week -> "week"
      | `File -> "file"
      | `Email -> "email"
      | `Image -> "image"
      | `Date -> "date"
      | `Datetime -> "datetime"
      | `Datetime_local -> "datetime-locale"
      | `Color -> "color"
      | `Button -> "button"
      | `Hidden -> "hidden"
    in user_attrib f "type" it

  let a_menu_type mt =
    let f = function
      | `Context -> "context"
      | `Toolbar -> "toolbar"
    in user_attrib f "type" mt

  let a_command_type ct =
    let f = function
      | `Command -> "command"
      | `Checkbox -> "checkbox"
      | `Radio -> "radio"
    in user_attrib f "type" ct

  let a_button_type bt =
    let f = function
      | `Button -> "button"
      | `Submit -> "submit"
      | `Reset -> "reset"
    in user_attrib f "type" bt

  let a_multiple x =
    let f = function
      | `Multiple -> "multiple"
    in user_attrib f "multiple" x

  let a_cols = int_attrib "cols"

  let a_rows = int_attrib "rows"

  let a_summary = string_attrib "summary"

  let a_align a =
    let f = function
      | `Left -> "left"
      | `Right -> "right"
      | `Justify -> "justify"
      | `Char -> "char"
    in user_attrib f "align" a

  let a_axis = string_attrib "axis"

  let a_colspan = int_attrib "colspan"

  let a_headers = space_sep_attrib "headers"

  let a_rowspan = int_attrib "rowspan"

  let a_scope s =
    let f = function
      | `Row -> "row"
      | `Col -> "col"
      | `Rowgroup -> "rowgroup"
      | `Colgroup -> "colgroup"
    in user_attrib f "scope" s

  let a_border = int_attrib "border"

  let a_cellpadding = length_attrib "cellpadding"

  let a_cellspacing = length_attrib "cellspacing"

  let a_datapagesize = string_attrib "datapagesize"

  let a_rules r =
    let f = function
      | `None -> "none"
      | `Groups -> "groups"
      | `Rows -> "rows"
      | `Cols -> "cols"
      | `All -> "all"
    in user_attrib f "rules" r

  let a_char c =
    user_attrib (String.make 1) "char" c

  let a_charoff = length_attrib "charoff"

  let a_data = uri_attrib "data"

  let a_codetype = string_attrib "codetype"

  let a_fs_rows mls = multilengths_attrib "rows" mls

  let a_fs_cols mls = multilengths_attrib "cols" mls

  let a_frameborder b =
    let f = function
      | `Zero -> "0"
      | `One -> "1"
    in user_attrib f "frameborder" b

  let a_marginheight = int_attrib "marginheight"

  let a_marginwidth = int_attrib "marginwidth"

  let a_scrolling s =
    let f = function
      | `Yes -> "yes"
      | `No -> "no"
      | `Auto -> "auto"
    in user_attrib f "scrolling" s

  let a_target = string_attrib "target"

  let a_content = string_attrib "content"

  let a_http_equiv = string_attrib "http-equiv"

  let a_media = mediadesc_attrib "media"

  type 'a elt = Xml.elt

  type html = [ | `Html ] elt

  (* NB: These are more general than the ones in xhtml.mli *)
  type ('a, 'b) nullary = ?a: (('a attrib) list) -> unit -> 'b elt

  type ('a, 'b, 'c) unary = ?a: (('a attrib) list) -> 'b elt wrap -> 'c elt

  type ('a, 'b, 'c, 'd) binary =
    ?a: (('a attrib) list) -> 'b elt wrap -> 'c elt wrap -> 'd elt

  type ('b, 'c, 'd, 'e) tri = 'b elt wrap -> 'c elt wrap -> 'd elt wrap -> 'e elt

  type ('a, 'b, 'c) star =
    ?a: (('a attrib) list) -> ('b elt) list wrap -> 'c elt

  type ('a, 'b, 'c) plus =
    ?a: (('a attrib) list) -> 'b elt wrap -> ('b elt) list wrap -> 'c elt

  let terminal tag ?a () = Xml.leaf ?a tag

  let nullary tag ?a () =
    Xml.node ?a tag (W.return [])

  let binary tag ?a elt1 elt2 =
    let l = W.fmap2 (fun x y -> [x; y]) elt1 elt2 in
    Xml.node ?a tag l

  let tri tag ?a elt1 elt2 elt3 =
    let l = W.fmap3 (fun x y z -> [x; y; z]) elt1 elt2 elt3 in
    Xml.node ?a tag l

  let unary tag ?a elt =
    Xml.node ?a tag W.(bind elt (fun x -> return [ x ]))

  let star tag ?a elts = Xml.node ?a tag elts

  let plus tag ?a elt elts =
    let l = W.fmap2 (fun x y -> x :: y) elt elts in
    Xml.node ?a tag l

  let plus_concat tag ?a elt elts =
    let l = W.fmap2 (@) elt elts in
    Xml.node ?a tag l

  let list_of_option = function
    | Some x -> [ x ]
    | None -> []

  let list_of_list_option = function
    | Some x -> x
    | None -> []

  let srcs_option = function
    | Some (`Srcs s) -> s
    | None -> []

  let phrasing_option = function
    | Some (`Phras p) -> p
    | None -> []

  let ruby_option = function
    | Some (`Rt_elt r) -> r
    | Some (`Group g) -> g
    | None -> []

  let body_option = function
    | Some (`Body b) -> b
    | Some (`Trs t) -> t
    | None -> []

  let colg_option = function
    | Some (`Colgroups c) -> c
    | None -> []

  let opts_option = function
    | Some (`Options o) -> o
    | Some (`Optgroups o) -> o
    | None -> []

  let li_option = function
    | Some (`Lis l) -> l
    | Some (`Flows f) -> f
    | None -> []

  let opt_option = function
    | Some (`Options o) -> o
    | Some (`Phras p) -> p
    | None -> []

  let param_option = function
    | Some (`Params p) -> p
    | None -> []

  let cols_option = function
    | Some (`Cols c) -> c
    | Some (`Colgroups c) -> c
    | None -> []

  let body = star "body"

  let head = plus "head"

  let title = unary "title"

  let html = binary "html"

  let footer = star "footer"

  let header = star "header"

  let section = star "section"

  let nav = star "nav"

  let pcdata = Xml.pcdata

  let entity = Xml.entity

  let space () = entity "nbsp"

  let cdata = Xml.cdata

  let cdata_script = Xml.cdata_script

  let cdata_style = Xml.cdata_style

  let h1 = star "h1"

  let h2 = star "h2"

  let h3 = star "h3"

  let h4 = star "h4"

  let h5 = star "h5"

  let h6 = star "h6"

  let hgroup = plus "hgroup"

  let address = star "address"

  let blockquote = star "blockquote"

  let div = star "div"

  let p = star "p"

  let pre = star "pre"

  let abbr = star "abbr"

  let br = terminal "br"

  let cite = star "cite"

  let code = star "code"

  let dfn = star "dfn"

  let em = star "em"

  let kbd = star "kbd"

  let q = star "q"

  let samp = star "samp"

  let span = star "span"

  let strong = star "strong"

  let time = star "time"

  let var = star "var"

  let a = star "a"

  let dl ?a list =
    let f l =
      List.concat
        (List.map
           (fun ((elt, elts), (elt', elts')) ->
              (elt :: elts) @ (elt' :: elts'))
           l)
    in
    Xml.node ?a "dl" (W.fmap f list)

  let ol = star "ol"

  let ul = star "ul"

  let dd = star "dd"

  let dt = star "dt"

  let li = star "li"

  let hr = terminal "hr"

  let b = star "b"

  let i = star "i"

  let u = star "u"

  let small = star "small"

  let sub = star "sub"

  let sup = star "sup"

  let mark = star "mark"

  let rp ?(a = []) elts = (a, elts)

  let rt ?rp ?a elts =
      match rp with
      | Some ((a1, e1), (a2, e2)) ->
          `Rpt (Xml.node ~a: a1 "rp" e1, Xml.node ?a "rt" elts,
            Xml.node ~a: a2 "rp" e2)
      | None -> `Rt (Xml.node ?a "rt" elts)

  let ruby ?a elt elts =
    let rec aux =
        function
        | [] -> []
        | (pel, `Rt e) :: l -> pel @ (e :: (aux l))
        | (pel, `Rpt (e1, e2, e3)) :: l -> pel @ (e1 :: e2 :: e3 :: (aux l))
    in
    let l = W.(bind elt (fun x -> bind elts (fun y -> return (x :: y))))
    in Xml.node ?a "ruby" (W.fmap aux l)

  let wbr = terminal "wbr"

    (* VB *)
    type shape = [ | `Rect | `Circle | `Poly | `Default ]

  let bdo ~dir ?(a = []) elts = Xml.node ~a: ((a_dir dir) :: a) "bdo" elts

  let a_datetime = string_attrib "datetime"

  let a_shape d =
      let f = function | `Rect -> "rect"
         | `Circle -> "circle"
         | `Poly -> "poly"
         | `Default -> "default"
 in user_attrib f "shape" d

  let a_coords coords =
    let f c = String.concat "," (List.map string_of_int c)
    in user_attrib f "coords" coords

  let a_usemap = string_attrib "usemap"

  let a_defer x =
    let f = function | `Defer -> "defer" in user_attrib f "defer" x

  let a_label = string_attrib "label"

  let area ~alt ?(a = []) () = Xml.leaf ~a: ((a_alt alt) :: a) "area"

  let map = plus "map"

  let del = star "del"

  let ins = star "ins"

  let script = unary "script"

  let noscript = plus "noscript"

  let article = star "article"

  let aside = star "aside"

  let video_audio name ?src ?(srcs=W.return []) ?(a = []) elts =
    let a =
      match src with
        | None -> a
        | Some uri -> (a_src uri) :: a
    in
    plus_concat ~a name srcs elts

  let audio = video_audio "audio"

  let video = video_audio "video"

  let canvas = star "canvas"

  let command ~label ?(a = []) () =
      Xml.leaf ~a: ((a_label label) :: a) "command"

  let menu ?child ?a () =
    let child = opt_fmap (fun x -> li_option (Some x)) child (li_option None) in
    Xml.node ?a "menu" child

  let embed = terminal "embed"

  let source = terminal "source"

  let meter = star "meter"

  let output_elt = star "output"

  let form = plus "form"

  let svg ?(xmlns = "http://www.w3.org/2000/svg") ?(a = []) children =
    star ~a:(string_attrib "xmlns" (W.return xmlns) ::(Svg.to_xmlattribs a))
      "svg" (W.fmap Svg.toeltl children)

  type input_attr =
    [ common
    | `Accept
    | `Alt
    | `Autocomplete
    | `Autofocus
    | `Checked
    | `Disabled
    | `Form
    | `Formation
    | `Formenctype
    | `Formmethod
    | `Formnovalidate
    | `Formtarget
    | `Height
    | `List
    | `Input_Max
    | `Maxlength
    | `Input_Min
    | `Multiple
    | `Name
    | `Pattern
    | `Placeholder
    | `ReadOnly
    | `Required
    | `Size
    | `Src
    | `Step
    | `Input_Type
    | `Value
    | `Width
    ]

  let input = terminal "input"

  let keygen = terminal "keygen"

  let label = star "label"

  let option = unary "option"

  let select = star "select"

  let textarea = unary "textarea"

  let button = star "button"

  let datalist ?children ?a () =
    let f = function `Options x | `Phras x -> x in
    Xml.node ?a "datalist" (opt_fmap f children [])

  let progress = star "proress"

  let legend = star "legend"

  let details summary ?a children =
    plus "details" ?a summary children

  let summary = star "summary"

  let fieldset ?legend ?a elts =
    plus_concat ?a "fieldset" (opt_fmap (fun x -> [ x ]) legend []) elts

  let optgroup ~label ?(a = []) elts =
    Xml.node ~a: ((a_label label) :: a) "optgroup" elts

  let figcaption = star "figcaption"
  let figure ?figcaption ?a elts =
    let add_caption caption elts = match caption with
      | None -> elts
      | Some x ->
          let f c elts = match c with
            | `Top figc -> figc :: elts
            | `Bottom figc -> elts @ [figc]
          in W.fmap2 f x elts
    in
    let content = add_caption figcaption elts in
    Xml.node ?a "figure" content

  let caption = star "caption"

  let tablex ?caption ?columns ?thead ?tfoot ?a elts =
    let thead = opt_fmap (fun x -> [ x ]) thead [] in
    let columns = opt_w columns [] in
    let tfoot = opt_fmap (fun x -> [ x ]) tfoot [] in
    let caption = opt_fmap (fun x -> [ x ]) caption [] in
    let f caption columns  thead tfoot l =
      caption @ columns @ thead @ tfoot @ l
    in Xml.node ?a "table" (W.fmap5 f caption columns thead tfoot elts)

  let table ?caption ?columns ?thead ?tfoot ?a elt elts =
    let l = W.fmap2 (fun x y -> x :: y) elt elts in
    tablex ?caption ?columns ?thead ?tfoot ?a l

  let td = star "td"

  let th = star "th"

  let tr = star "tr"

  let colgroup = star "colgroup"

  let col = terminal "col"

  let thead = star "thead"

  let tbody = star "tbody"

  let tfoot = star "tfoot"

  let iframe = star "iframe"

  let object_ ?params ?(a = []) elts =
    plus_concat ~a "object" (opt_w params []) elts

  let param = terminal "param"

  let img ~src ~alt ?(a = []) () =
    let a = (a_src src) :: (a_alt alt) :: a in
      Xml.leaf ~a "img"

  let meta = terminal "meta"

  let style ?(a = []) elts = Xml.node ~a "style" elts

  let link ~rel ~href ?(a = []) () =
    Xml.leaf ~a: ((a_rel rel) :: (a_href href) :: a) "link"

  let base = terminal "base"

  (* VB *)

  type rt =
    [ `Rt of [ | `Rt ] elt
    | `Rpt of (([ | `Rp ] elt) * ([ | `Rt ] elt) * ([ | `Rp ] elt))
    ]

  type ruby_content = (((phrasing elt) list) * rt)

  type rp = (((common attrib) list) * ((phrasing elt) list wrap))

  (******************************************************************)
  (* Conversion from and to Xml module *)
  let tot x = x

  let totl x = x

  let toelt x = x

  let toeltl x = x

  type doc  = [ `Html ] elt
  let doc_toelt x = x

  module Unsafe = struct

    let data s = Xml.encodedpcdata s

    let leaf tag ?a () = Xml.leaf ?a tag

    let node tag ?a elts = Xml.node ?a tag elts

    let coerce_elt x = x

    let float_attrib = Xml.float_attrib

    let int_attrib = Xml.int_attrib

    let string_attrib = Xml.string_attrib

    let uri_attrib a s = Xml.uri_attrib a s

    let space_sep_attrib = Xml.space_sep_attrib

    let comma_sep_attrib = Xml.comma_sep_attrib

    let event_handler_attrib = Xml.event_handler_attrib

  end

end

module Make
    (Xml : Xml_sigs.T)
    (Svg : Svg_sigs.T with module Xml := Xml) =
  MakeWrapped
    (Xml_wrap.NoWrap)
    (Xml)
    (Svg)
