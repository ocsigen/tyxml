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

open HTML5_types

module Make(XML : XML_sigs.T)(SVG : SVG_sigs.T with module XML := XML) = struct

  module Info = struct
    let content_type = "text/html"
    let version = "HTML5-draft"
    let standard = Uri.uri_of_string "http://www.w3.org/TR/html5/"
    let doctype =
      XML_print.compose_doctype "html" []
    let emptytags =
      [ "area"; "base"; "br"; "col"; "command"; "embed"; "hr"; "img";
        "input"; "keygen"; "link"; "meta"; "param"; "source"; "wbr" ]
  end

  include Uri
  type 'a attrib = XML.attrib

  let to_xmlattribs x = x
  let to_attrib x = x

  (* VB *)
  let float_attrib = XML.float_attrib

  let int_attrib = XML.int_attrib

  let string_attrib = XML.string_attrib

  let uri_attrib a s = XML.string_attrib a (string_of_uri s)

  let space_sep_attrib = XML.space_sep_attrib

  let comma_sep_attrib = XML.comma_sep_attrib

  let event_attrib = XML.event_attrib

  (* space-separated *)
  let length_attrib name =
    function
    | `Pixels p -> int_attrib name p
    | `Percent p -> string_attrib name ((string_of_int p) ^ "%")

  let multilength_attrib name =
    function
    | (#length as l) -> length_attrib name l
    | `Relative 1 -> string_attrib name "*"
    | `Relative i -> string_attrib name ((string_of_int i) ^ "*")

  let multilength_to_string =
    function
    | `Pixels p -> string_of_int p
    | `Percent p -> (string_of_int p) ^ "%"
    | `Relative 1 -> "*"
    | `Relative i -> (string_of_int i) ^ "*"

  let multilengths_attrib name multilengths =
    string_attrib name
      (String.concat ", " (List.map multilength_to_string multilengths))

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

  let linktypes_attrib name linktypes =
    string_attrib name
      (String.concat " " (List.map linktype_to_string linktypes))

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

  let mediadesc_attrib name mediadescs =
    string_attrib name
      (String.concat ", " (List.map mediadesc_to_string mediadescs))

  (* Core: *)
  let a_class = space_sep_attrib "class"

  let a_id = string_attrib "id"

  let a_user_data name = string_attrib ("data-" ^ name)

  let a_title = string_attrib "title"

  (* I18N: *)
  let a_xml_lang = string_attrib "xml:lang"

  (* Style: *)
  let a_style = string_attrib "style"

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

  let a_xmlns =
      function
      | `W3_org_1999_xhtml ->
          string_attrib "xmlns" "http://www.w3.org/1999/xhtml"

  let a_manifest = uri_attrib "manifest"

  let a_cite = uri_attrib "cite"

  let a_xml_space =
      function | `Preserve -> string_attrib "xml:space" "preserve"

  let a_accesskey c = string_attrib "accesskey" (String.make 1 c)

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

  let a_selected =
      function | `Selected -> string_attrib "selected" "selected"

  let a_text_value = string_attrib "value"

  let a_int_value = int_attrib "value"

  let a_value = string_attrib "value"

  let a_float_value = float_attrib "value"

  let a_action = uri_attrib "action"

  let a_method m =
      string_attrib "method"
        (match m with
         | `Get -> "GET"
         | `Post -> "POST"
         | `Put -> "PUT"
         | `Delete -> "DELETE")

  let a_enctype = string_attrib "enctype"

  let a_checked = function | `Checked -> string_attrib "checked" "checked"

  let a_disabled =
      function | `Disabled -> string_attrib "disabled" "disabled"

  let a_readonly =
      function | `Readonly -> string_attrib "readonly" "readonly"

  let a_maxlength = int_attrib "maxlength"

  let a_name = string_attrib "name"

  let a_autocomplete ac =
      string_attrib "autocomplete"
        (match ac with | `On -> "on" | `Off -> "off")

  let a_async = function | `Async -> string_attrib "async" "async"

  let a_autofocus =
      function | `Autofocus -> string_attrib "autofocus" "autofocus"

  let a_autoplay =
      function | `Autoplay -> string_attrib "autoplay" "autoplay"

  let a_challenge = string_attrib "challenge"

  let a_contenteditable ce =
      string_attrib "contexteditable"
        (match ce with | `True -> "true" | `False -> "false")

  let a_contextmenu = string_attrib "contextmenu"

  let a_controls =
      function | `Controls -> string_attrib "controls" "controls"

  let a_dir d =
      string_attrib "dir" (match d with | `Ltr -> "ltr" | `Rtl -> "rtl")

  let a_draggable d =
      string_attrib "draggable"
        (match d with | `True -> "true" | `False -> "false")

  let a_form = string_attrib "form"

  let a_formaction = uri_attrib "formaction"

  let a_formenctype = string_attrib "formenctype"

  let a_formmethod m =
      string_attrib "method"
        (match m with
         | `Get -> "GET"
         | `Post -> "POST"
         | `Put -> "PUT"
         | `Delete -> "DELETE")

  let a_formnovalidate =
      function
      | `Formnovalidate -> string_attrib "formnovalidate" "formnovalidate"

  let a_formtarget = string_attrib "formtarget"

  let a_hidden = function | `Hidden -> string_attrib "hidden" "hidden"

  let a_high = float_attrib "high"

  let a_icon = uri_attrib "icon"

  let a_ismap = function | `Ismap -> string_attrib "ismap" "ismap"

  let a_keytype = string_attrib "keytype"

  let a_list = string_attrib "list"

  let a_loop = function | `Loop -> string_attrib "loop" "loop"

  let a_low = float_attrib "low"

  let a_max = float_attrib "max"

  let a_input_max = int_attrib "max"

  let a_min = float_attrib "min"

  let a_input_min = int_attrib "min"

  let a_novalidate =
      function | `Novalidate -> string_attrib "novalidate" "novalidate"

  let a_open = function | `Open -> string_attrib "open" "open"

  let a_optimum = float_attrib "optimum"

  let a_pattern = string_attrib "pattern"

  let a_placeholder = string_attrib "placeholder"

  let a_poster = uri_attrib "poster"

  let a_preload pl =
      string_attrib "preload"
        (match pl with
         | `None -> "none"
         | `Metadata -> "metadata"
         | `Audio -> "audio")

  let a_pubdate = function | `Pubdate -> string_attrib "pubdate" "pubdate"

  let a_radiogroup = string_attrib "radiogroup"

  let a_required =
      function | `Required -> string_attrib "required" "required"

  let a_reversed =
      function | `Reversed -> string_attrib "reserved" "reserved"

  let rec a_sandbox sb =
    let rec aux sb =
        match sb with
        | `AllowSameOrigin :: a -> "allow-same-origin" :: (aux a)
        | `AllowForms :: a -> "allow-forms" :: (aux a)
        | `AllowScript :: a -> "allow-script" :: (aux a)
        | [] -> []
      in space_sep_attrib "sandbox" (aux sb)

  let a_spellcheck sc =
      string_attrib "spellckeck"
        (match sc with | `True -> "true" | `False -> "false")

  let a_scoped = function | `Scoped -> string_attrib "scoped" "scoped"

  let a_seamless =
      function | `Seamless -> string_attrib "seamless" "seamless"

  let a_sizes sizes =
      string_attrib "sizes"
        (String.concat " " (List.map string_of_int sizes))

  let a_span = int_attrib "span"

    (*let a_srcdoc*)
  let a_srclang = string_attrib "xml:lang"

  let a_start = int_attrib "start"

  let a_step = float_attrib "step"

  let a_wrap w =
      string_attrib "wrap" (match w with | `Soft -> "soft" | `Hard -> "hard")

  let a_size = int_attrib "size"

  let a_input_type it =
      string_attrib "type"
        (match it with
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
         | `Hidden -> "hidden")

  let a_menu_type mt =
      string_attrib "type"
        (match mt with | `Context -> "context" | `Toolbar -> "toolbar")

  let a_command_type ct =
      string_attrib "type"
        (match ct with
         | `Command -> "command"
         | `Checkbox -> "checkbox"
         | `Radio -> "radio")

  let a_button_type bt =
      string_attrib "type"
        (match bt with
         | `Button -> "button"
         | `Submit -> "submit"
         | `Reset -> "reset")

  let a_multiple =
      function | `Multiple -> string_attrib "multiple" "multiple"

  let a_cols = int_attrib "cols"

  let a_rows = int_attrib "rows"

  let a_summary = string_attrib "summary"

  let a_align a =
      string_attrib "align"
        (match a with
         | `Left -> "left"
         | `Right -> "right"
         | `Justify -> "justify"
         | `Char -> "char")

  let a_axis = string_attrib "axis"

  let a_colspan = int_attrib "colspan"

  let a_headers = space_sep_attrib "headers"

  let a_rowspan = int_attrib "rowspan"

  let a_scope s =
      string_attrib "scope"
        (match s with
         | `Row -> "row"
         | `Col -> "col"
         | `Rowgroup -> "rowgroup"
         | `Colgroup -> "colgroup")

  let a_border = int_attrib "border"

  let a_cellpadding = length_attrib "cellpadding"

  let a_cellspacing = length_attrib "cellspacing"

  let a_datapagesize = string_attrib "datapagesize"

  let a_rules r =
      string_attrib "rules"
        (match r with
         | `None -> "none"
         | `Groups -> "groups"
         | `Rows -> "rows"
         | `Cols -> "cols"
         | `All -> "all")

  let a_char c = string_attrib "char" (String.make 1 c)

  let a_charoff = length_attrib "charoff"

  let a_data = uri_attrib "data"

  let a_codetype = string_attrib "codetype"

  let a_fs_rows mls = multilengths_attrib "rows" mls

  let a_fs_cols mls = multilengths_attrib "cols" mls

  let a_frameborder b =
      int_attrib "frameborder" (match b with | `Zero -> 0 | `One -> 1)

  let a_marginheight = int_attrib "marginheight"

  let a_marginwidth = int_attrib "marginwidth"

  let a_scrolling s =
      string_attrib "scrolling"
        (match s with | `Yes -> "yes" | `No -> "no" | `Auto -> "auto")

  let a_target = string_attrib "target"

  let a_content = string_attrib "content"

  let a_http_equiv = string_attrib "http-equiv"

  let a_media = mediadesc_attrib "media"

    type 'a elt = XML.elt

    type html = [ | `Html ] elt

    (* NB: These are more general than the ones in xHTML.mli *)
    type ('a, 'b) nullary = ?a: (('a attrib) list) -> unit -> 'b elt

    type ('a, 'b, 'c) unary = ?a: (('a attrib) list) -> 'b elt -> 'c elt

    type ('a, 'b, 'c, 'd) binary =
      ?a: (('a attrib) list) -> 'b elt -> 'c elt -> 'd elt

    type ('b, 'c, 'd, 'e) tri = 'b elt -> 'c elt -> 'd elt -> 'e elt

    type ('a, 'b, 'c) star =
      ?a: (('a attrib) list) -> ('b elt) list -> 'c elt

    type ('a, 'b, 'c) plus =
      ?a: (('a attrib) list) -> 'b elt -> ('b elt) list -> 'c elt

  let terminal tag ?a () = XML.leaf ?a tag

    (* let nullary tag ?a () = XML.node ?a tag [] *)
  let unary tag ?a elt = XML.node ?a tag [ elt ]

  let binary tag ?a elt1 elt2 = XML.node ?a tag [ elt1; elt2 ]

  let tri tag elt1 elt2 elt3 = XML.node tag [ elt1; elt2; elt3 ]

  let star tag ?a elts = XML.node ?a tag elts

  let plus tag ?a elt elts = XML.node ?a tag (elt :: elts)

  let list_of_option = function | Some x -> [ x ] | None -> []

  let list_of_list_option = function | Some x -> x | None -> []

  let srcs_option = function | Some (`Srcs s) -> s | None -> []

  let phrasing_option = function | Some (`Phras p) -> p | None -> []

  let ruby_option =
      function | Some (`Rt_elt r) -> r | Some (`Group g) -> g | None -> []

  let body_option =
      function | Some (`Body b) -> b | Some (`Trs t) -> t | None -> []

  let colg_option = function | Some (`Colgroups c) -> c | None -> []

  let opts_option =
      function
      | Some (`Options o) -> o
      | Some (`Optgroups o) -> o
      | None -> []

  let li_option =
      function | Some (`Lis l) -> l | Some (`Flows f) -> f | None -> []

  let opt_option =
      function | Some (`Options o) -> o | Some (`Phras p) -> p | None -> []

  let param_option = function | Some (`Params p) -> p | None -> []

  let cols_option =
      function | Some (`Cols c) -> c | Some (`Colgroups c) -> c | None -> []

  let body = star "body"

  let head = plus "head"

  let title = unary "title"

  let html = binary "html"

  let footer = star "footer"

  let header = star "header"

  let section = star "section"

  let nav = star "nav"

  let pcdata = XML.pcdata

  let entity = XML.entity

  let space () = entity "nbsp"

  let cdata = XML.cdata

  let cdata_script = XML.cdata_script

  let cdata_style = XML.cdata_style

  let unsafe_data s = XML.encodedpcdata s

  let unsafe_data s = XML.encodedpcdata s

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
      XML.node ?a "dl"
        (List.concat
           (List.map
              (fun ((elt, elts), (elt', elts')) ->
                 (elt :: elts) @ (elt' :: elts'))
              list))

  let ol = star "ol"

  let ul = star "ul"

  let dd = star "dd"

  let dt = star "dt"

  let li = star "li"

  let hr = terminal "hr"

  let b = star "b"

  let i = star "i"

  let small = star "small"

  let sub = star "sub"

  let sup = star "sup"

  let mark = star "mark"

  let rp ?(a = []) elts = (a, elts)

  let rt ?rp ?a elts =
      match rp with
      | Some ((a1, e1), (a2, e2)) ->
          `Rpt (XML.node ~a: a1 "rp" e1, XML.node ?a "rt" elts,
            XML.node ~a: a2 "rp" e2)
      | None -> `Rt (XML.node ?a "rt" elts)

  let ruby ?a elt elts =
    let rec aux =
        function
        | [] -> []
        | (pel, `Rt e) :: l -> pel @ (e :: (aux l))
        | (pel, `Rpt (e1, e2, e3)) :: l -> pel @ (e1 :: e2 :: e3 :: (aux l))
      in XML.node ?a "ruby" (aux (elt :: elts))

  let wbr = terminal "wbr"

    (* VB *)
    type shape = [ | `Rect | `Circle | `Poly | `Default ]

  let bdo ~dir ?(a = []) elts = XML.node ~a: ((a_dir dir) :: a) "bdo" elts

  let a_datetime = string_attrib "datetime"

  let a_shape d =
      string_attrib "shape"
        (match d with
         | `Rect -> "rect"
         | `Circle -> "circle"
         | `Poly -> "poly"
         | `Default -> "default")

  let a_coords coords =
      string_attrib "coords"
        (String.concat "," (List.map string_of_int coords))

  let a_usemap = string_attrib "usemap"

  let a_defer = function | `Defer -> string_attrib "defer" "defer"

  let a_label = string_attrib "label"

  let area ~alt ?(a = []) () = XML.leaf ~a: ((a_alt alt) :: a) "area"

  let map = plus "map"

  let del = star "del"

  let ins = star "ins"

  let script ?(a = []) elt = XML.node ~a "script" [ elt ]

  let noscript = plus "noscript"

  let article = star "article"

  let aside = star "aside"

  let video_audio name ?srcs ?(a = []) elts =
    let (a, children) =
        match srcs with
        | None -> (a, elts)
        | Some (uri, srcs) -> (((a_src uri) :: a), (srcs @ elts))
      in XML.node ~a name children

  let audio = video_audio "audio"

  let video = video_audio "video"

  let canvas = star "canvas"

  let command ~label ?(a = []) () =
      XML.leaf ~a: ((a_label label) :: a) "command"

  let menu ?child ?a () = XML.node ?a "menu" (li_option child)

  let embed = terminal "embed"

  let source = terminal "source"

  let meter = star "meter"

  let output_elt = star "output"

  let form = plus "form"

  let svg ?(xmlns = "http://www.w3.org/2000/svg")
      ?(a = []) children = star ~a:(string_attrib "xmlns" xmlns ::(SVG.to_xmlattribs a))
      "svg" (SVG.toeltl children)
  type input_attr =
      [
        | common
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
    let children =
        match children with
        | Some (`Options x) -> x
        | Some (`Phras x) -> x
        | None -> []
      in XML.node ?a "datalist" children

  let progress = star "proress"

  let legend = star "legend"

  let details summary ?a children =
      XML.node "details" ?a (summary :: children)

  let summary = star "summary"

  let fieldset ?legend ?a elts =
      XML.node ?a "fieldset" ((list_of_option legend) @ elts)

  let optgroup ~label ?(a = []) elts =
      XML.node ~a: ((a_label label) :: a) "optgroup" elts

  let figcaption = star "figcaption"

  let figure ?figcaption ?a elts =
      XML.node ?a "figure" ((list_of_option figcaption) @ elts)

  let caption = star "caption"

  let table ?caption ?(columns = []) ?thead ?tfoot ?a elt elts =
      XML.node ?a "table"
        ((list_of_option caption) @
           (columns @
              ((list_of_option thead) @
                 ((list_of_option tfoot) @ (elt :: elts)))))

  let tablex ?caption ?(columns = []) ?thead ?tfoot ?a elts =
      XML.node ?a "table"
        ((list_of_option caption) @
           (columns @
              ((list_of_option thead) @ ((list_of_option tfoot) @ elts))))

  let td = star "td"

  let th = star "th"

  let tr = star "tr"

  let colgroup = star "colgroup"

  let col = terminal "col"

  let thead = star "thead"

  let tbody = star "tbody"

  let tfoot = star "tfoot"

  let iframe = star "iframe"

  let object_ ?(params = []) ?(a = []) elts =
      XML.node ~a "object" (params @ elts)

  let param = terminal "param"

  let img ~src ~alt ?(a = []) () =
    let a = (a_src src) :: (a_alt alt) :: a in
      XML.leaf ~a "img"

  let meta = terminal "meta"

  let style ?(a = []) elts = XML.node ~a "style" elts

  let link ~rel ~href ?(a = []) () =
      XML.leaf ~a: ((a_rel rel) :: (a_href href) :: a) "link"

  let base = terminal "base"

    (* VB *)

    type rt =
      [
        | `Rt of [ | `Rt ] elt
        | `Rpt of (([ | `Rp ] elt) * ([ | `Rt ] elt) * ([ | `Rp ] elt))
      ]

    type ruby_content = (((phrasing elt) list) * rt)

    type rp = (((common attrib) list) * ((phrasing elt) list))

    (******************************************************************)
    (* In the following, my own stuffs for Ocsigen -- Vincent: *)
  let tot x = x

  let totl x = x

  let toelt x = x

  let toeltl x = x

  type doc  = [ `Html ] elt
  let doc_toelt x = x

end
