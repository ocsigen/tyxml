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
    (Xml : Xml_sigs.Wrapped with type 'a wrap = 'a W.t
                             and type 'a list_wrap = 'a W.tlist)
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
  type 'a list_wrap = 'a W.tlist

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

  let bool_attrib = user_attrib string_of_bool

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
  let a_onabort = Xml.event_handler_attrib "onabort"
  let a_onafterprint = Xml.event_handler_attrib "onafterprint"
  let a_onbeforeprint = Xml.event_handler_attrib "onbeforeprint"
  let a_onbeforeunload = Xml.event_handler_attrib "onbeforeunload"
  let a_onblur = Xml.event_handler_attrib "onblur"
  let a_oncanplay = Xml.event_handler_attrib "oncanplay"
  let a_oncanplaythrough = Xml.event_handler_attrib "oncanplaythrough"
  let a_onchange = Xml.event_handler_attrib "onchange"
  let a_ondurationchange = Xml.event_handler_attrib "ondurationchange"
  let a_onemptied = Xml.event_handler_attrib "onemptied"
  let a_onended = Xml.event_handler_attrib "onended"
  let a_onerror = Xml.event_handler_attrib "onerror"
  let a_onfocus = Xml.event_handler_attrib "onfocus"
  let a_onformchange = Xml.event_handler_attrib "onformchange"
  let a_onforminput = Xml.event_handler_attrib "onforminput"
  let a_onhashchange = Xml.event_handler_attrib "onhashchange"
  let a_oninput = Xml.event_handler_attrib "oninput"
  let a_oninvalid = Xml.event_handler_attrib "oninvalid"
  let a_onoffline = Xml.event_handler_attrib "onoffline"
  let a_ononline = Xml.event_handler_attrib "ononline"
  let a_onpause = Xml.event_handler_attrib "onpause"
  let a_onplay = Xml.event_handler_attrib "onplay"
  let a_onplaying = Xml.event_handler_attrib "onplaying"
  let a_onpagehide = Xml.event_handler_attrib "onpagehide"
  let a_onpageshow = Xml.event_handler_attrib "onpageshow"
  let a_onpopstate = Xml.event_handler_attrib "onpopstate"
  let a_onprogress = Xml.event_handler_attrib "onprogress"
  let a_onratechange = Xml.event_handler_attrib "onratechange"
  let a_onreadystatechange = Xml.event_handler_attrib "onreadystatechange"
  let a_onredo = Xml.event_handler_attrib "onredo"
  let a_onresize = Xml.event_handler_attrib "onresize"
  let a_onscroll = Xml.event_handler_attrib "onscroll"
  let a_onseeked = Xml.event_handler_attrib "onseeked"
  let a_onseeking = Xml.event_handler_attrib "onseeking"
  let a_onselect = Xml.event_handler_attrib "onselect"
  let a_onshow = Xml.event_handler_attrib "onshow"
  let a_onstalled = Xml.event_handler_attrib "onstalled"
  let a_onstorage = Xml.event_handler_attrib "onstorage"
  let a_onsubmit = Xml.event_handler_attrib "onsubmit"
  let a_onsuspend = Xml.event_handler_attrib "onsuspend"
  let a_ontimeupdate = Xml.event_handler_attrib "ontimeupdate"
  let a_onundo = Xml.event_handler_attrib "onundo"
  let a_onunload = Xml.event_handler_attrib "onunload"
  let a_onvolumechange = Xml.event_handler_attrib "onvolumechange"
  let a_onwaiting = Xml.event_handler_attrib "onwaiting"
  let a_onload = Xml.event_handler_attrib "onload"
  let a_onloadeddata = Xml.event_handler_attrib "onloadeddata"
  let a_onloadedmetadata = Xml.event_handler_attrib ""
  let a_onloadstart = Xml.event_handler_attrib "onloadstart"
  let a_onmessage = Xml.event_handler_attrib "onmessage"
  let a_onmousewheel = Xml.event_handler_attrib "onmousewheel"

  (** Javascript mouse events *)
  let a_onclick = Xml.mouse_event_handler_attrib "onclick"
  let a_oncontextmenu = Xml.mouse_event_handler_attrib "oncontextmenu"
  let a_ondblclick = Xml.mouse_event_handler_attrib "ondblclick"
  let a_ondrag = Xml.mouse_event_handler_attrib "ondrag"
  let a_ondragend = Xml.mouse_event_handler_attrib "ondragend"
  let a_ondragenter = Xml.mouse_event_handler_attrib "ondragenter"
  let a_ondragleave = Xml.mouse_event_handler_attrib "ondragleave"
  let a_ondragover = Xml.mouse_event_handler_attrib "ondragover"
  let a_ondragstart = Xml.mouse_event_handler_attrib "ondragstart"
  let a_ondrop = Xml.mouse_event_handler_attrib "ondrop"
  let a_onmousedown = Xml.mouse_event_handler_attrib "onmousedown"
  let a_onmouseup = Xml.mouse_event_handler_attrib "onmouseup"
  let a_onmouseover = Xml.mouse_event_handler_attrib "onmouseover"
  let a_onmousemove = Xml.mouse_event_handler_attrib "onmousemove"
  let a_onmouseout = Xml.mouse_event_handler_attrib "onmouseout"

  (** Javascript keyboard events *)
  let a_onkeypress = Xml.keyboard_event_handler_attrib "onkeypress"
  let a_onkeydown = Xml.keyboard_event_handler_attrib "onkeydown"
  let a_onkeyup = Xml.keyboard_event_handler_attrib "onkeyup"

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

  let a_muted x =
    let f = function
      | `Muted -> "muted"
    in user_attrib f "muted" x

  let a_crossorigin x =
    let f = function
      | `Anonymous -> "anonymous"
      | `Use_credentials -> "use-credentials"
    in user_attrib f "crossorigin" x

  let a_mediagroup = string_attrib "mediagroup"

  let a_challenge = string_attrib "challenge"

  let a_contenteditable ce =
    bool_attrib "contenteditable" ce

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
    bool_attrib "draggable" d

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

  let a_sandbox sb =
    let rec aux sb =
      match sb with
      | `AllowSameOrigin :: a -> "allow-same-origin" :: (aux a)
      | `AllowForms :: a -> "allow-forms" :: (aux a)
      | `AllowScript :: a -> "allow-script" :: (aux a)
      | [] -> []
    in space_sep_attrib "sandbox" (W.fmap aux sb)

  let a_spellcheck sc =
    bool_attrib "spellckeck" sc

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

  type ('a, 'b) nullary = ?a: (('a attrib) list) -> unit -> 'b elt

  type ('a, 'b, 'c) unary = ?a: (('a attrib) list) -> 'b elt wrap -> 'c elt

  type ('a, 'b, 'c) star =
    ?a: (('a attrib) list) -> ('b elt) list_wrap -> 'c elt

  let terminal tag ?a () = Xml.leaf ?a tag

  let unary tag ?a elt =
    Xml.node ?a tag (W.singleton elt)

  let star tag ?a elts = Xml.node ?a tag elts

  let plus tag ?a elt elts =
    Xml.node ?a tag (W.cons elt elts)

  let option_cons opt elts =
    match opt with
    | None -> elts
    | Some x -> W.cons x elts

  let body = star "body"

  let head = plus "head"

  let title = unary "title"

  let html ?a head body =
    let content = W.cons head (W.singleton body) in
    Xml.node ?a "html" content

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

  let hgroup = star "hgroup"

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

  let dl = star "dl"

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

  let rp = star "rp"

  let rt = star "rt"

  let ruby = star "ruby"

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

  let map = star "map"

  let del = star "del"

  let ins = star "ins"

  let script = unary "script"

  let noscript = star "noscript"

  let article = star "article"

  let aside = star "aside"

  let video_audio name ?src ?srcs ?(a = []) elts =
    let a =
      match src with
      | None -> a
      | Some uri -> (a_src uri) :: a
    in
    match srcs with
    | None -> Xml.node name ~a elts
    | Some srcs -> Xml.node name ~a (W.append srcs elts)

  let audio = video_audio "audio"

  let video = video_audio "video"

  let canvas = star "canvas"

  let command ~label ?(a = []) () =
    Xml.leaf ~a: ((a_label label) :: a) "command"

  let menu ?child ?a () =
    let child = match child with
      | None -> W.nil ()
      | Some (`Lis l)
      | Some (`Flows l) -> l in
    Xml.node ?a "menu" child

  let embed = terminal "embed"

  let source = terminal "source"

  let meter = star "meter"

  let output_elt = star "output"

  let form = star "form"

  let svg ?(xmlns = "http://www.w3.org/2000/svg") ?(a = []) children =
    star ~a:(string_attrib "xmlns" (W.return xmlns) ::(Svg.to_xmlattribs a))
      "svg" (W.map Svg.toelt children)

  let input = terminal "input"

  let keygen = terminal "keygen"

  let label = star "label"

  let option = unary "option"

  let select = star "select"

  let textarea = unary "textarea"

  let button = star "button"

  let datalist ?children ?a () =
    let children = match children with
      | None -> W.nil ()
      | Some (`Options x | `Phras x) -> x in
    Xml.node ?a "datalist" children

  let progress = star "progress"

  let legend = star "legend"

  let details summary ?a children =
    plus "details" ?a summary children

  let summary = star "summary"

  let fieldset ?legend ?a elts =
    Xml.node ?a "fieldset" (option_cons legend elts)

  let optgroup ~label ?(a = []) elts =
    Xml.node ~a: ((a_label label) :: a) "optgroup" elts

  let figcaption = star "figcaption"
  let figure ?figcaption ?a elts =
    let content = match figcaption with
      | None -> elts
      | Some (`Top c) -> W.cons c elts
      | Some (`Bottom c) -> W.append elts (W.singleton c)
    in
    Xml.node ?a "figure" content

  let caption = star "caption"

  let tablex ?caption ?columns ?thead ?tfoot ?a elts =
    let content = option_cons thead (option_cons tfoot elts) in
    let content = match columns with
      | None -> content
      | Some columns -> W.append columns content in
    let content = option_cons caption content in
    Xml.node ?a "table" content

  let table = tablex

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
    let elts = match params with
      | None -> elts
      | Some e -> W.append e elts in
    Xml.node ~a "object" elts

  let param = terminal "param"

  let img ~src ~alt ?(a = []) () =
    let a = (a_src src) :: (a_alt alt) :: a in
    Xml.leaf ~a "img"

  let meta = terminal "meta"

  let style ?(a = []) elts = Xml.node ~a "style" elts

  let link ~rel ~href ?(a = []) () =
    Xml.leaf ~a: ((a_rel rel) :: (a_href href) :: a) "link"

  let base = terminal "base"

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

  end

end

module Make
    (Xml : Xml_sigs.T)
    (Svg : Svg_sigs.T with module Xml := Xml) =
  MakeWrapped
    (Xml_wrap.NoWrap)
    (Xml)
    (Svg)
