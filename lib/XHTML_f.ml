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

(* IDEAS:
      The [a_] prefix would have to be maintained and the
   only advantage are a potentially better mapping of the XHTML modularization
   to O'Caml modules. *)

(** Typesafe constructors for XHTML 1.1 documents.
    @see <http://www.w3.org/TR/xhtml-modularization/abstract_modules.html> W3C Recommendation *)

open XHTML_types

module Version(XML : XML_sigs.T) = struct

  module XML = XML
  include Uri
  (* Directly from http://www.w3.org/TR/xhtml-modularization/abstract_modules.html *)

  type core = [ `Class | `Id | `Title | `XML_space ]
  type i18n = [ `XML_lang | `Dir ]

  type events =
      [ `OnClick | `OnDblClick | `OnMouseDown | `OnMouseUp |
        `OnMouseOver | `OnMouseMove | `OnMouseOut | `OnKeyPress |
	`OnKeyDown | `OnKeyUp ]

  type common = [ core | i18n | events | `Style_Attr]

  type 'a attrib = XML.attrib

  let to_xmlattribs x = x (* VB *)
  let to_attrib x = x

  let int_attrib = XML.int_attrib
  let string_attrib = XML.string_attrib
  let uri_attrib a b = XML.string_attrib a (string_of_uri b)
  let space_sep_attrib = XML.space_sep_attrib
  let comma_sep_attrib = XML.comma_sep_attrib
  let event_attrib = XML.event_attrib

  type cdata = string
  type id = string
  type idref = string
  type idrefs = idref list (* space-separated *)
  type name = string
  type nmtoken = string
  type nmtokens = nmtoken list (* space-separated *)
  type pcdata = string

  type character = char
  type charset = string
  type charsets = charset list (* space-separated *)

  type contenttype = string
  type contenttypes = contenttype list (* comma-separated *)
  type coords = string list (* Comma separated list of coordinates to use in defining areas. *)
  type datetime = string
  type fpi = string
  type frametarget = string
  type languagecode = string
  type length = [ `Pixels of int | `Percent of int ]
  type linktypes =
      [`Alternate | `Appendix | `Bookmark | `Chapter | `Contents
    | `Copyright | `Glossary | `Help | `Index | `Next | `Prev
    | `Section | `Start | `Stylesheet | `Subsection | `Other of string] list
  type mediadesc =
      [ `All | `Aural | `Braille | `Embossed | `Handheld | `Print
    | `Projection | `Screen | `Speech | `TTY | `TV ] list

  type multilength = [ length | `Relative of int ]
  type multilengths = multilength list (* comma-separated *)
  type number = int
  type pixels = int
  type script = string
  type text = string

  let length_attrib name = function
    | `Pixels p -> int_attrib name p
    | `Percent p -> string_attrib name (string_of_int p ^ "%")

  let multilength_attrib name = function
    | #length as l -> length_attrib name l
    | `Relative 1 -> string_attrib name "*"
    | `Relative i -> string_attrib name (string_of_int i ^ "*")

  let multilength_to_string = function
    | `Pixels p -> string_of_int p
    | `Percent p -> string_of_int p ^ "%"
    | `Relative 1 -> "*"
    | `Relative i -> string_of_int i ^ "*"

  let multilengths_attrib name multilengths =
    string_attrib name
      (String.concat ", " (List.map multilength_to_string multilengths))

  let linktype_to_string = function
    | `Alternate -> "alternate"
    | `Appendix -> "appendix"
    | `Bookmark -> "bookmark"
    | `Chapter -> "chapter"
    | `Contents -> "contents"
    | `Copyright -> "copyright"
    | `Glossary -> "glossary"
    | `Help -> "help"
    | `Index -> "index"
    | `Next -> "next"
    | `Prev -> "prev"
    | `Section -> "section"
    | `Start -> "start"
    | `Stylesheet -> "stylesheet"
    | `Subsection -> "subsection"
    | `Other t -> t

  let linktypes_attrib name linktypes =
    string_attrib name
      (String.concat " " (List.map linktype_to_string linktypes))

  let mediadesc_to_string = function
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
      (* class is different on client side.
         We put the value in xML.ml
         because this file has a different implementation client side.
       *)
  let a_id = string_attrib "id"
  let a_title = string_attrib "title"

      (* I18N: *)

  let a_xml_lang = string_attrib "xml:lang"

      (* Style: *)

  let a_style = string_attrib "style"


      (* Events: *)

  let a_onblur = event_attrib "onblur"
  let a_onclick = event_attrib "onclick"
  let a_ondblclick = event_attrib "ondblclick"
  let a_onchange = event_attrib "onchange"
  let a_onfocus = event_attrib "onfocus"
  let a_onload = event_attrib "onload"
  let a_onunload = event_attrib "onunload"
  let a_onreset = event_attrib "onreset"
  let a_onselect = event_attrib "onselect"
  let a_onsubmit = event_attrib "onsubmit"
  let a_onmousedown = event_attrib "onmousedown"
  let a_onmouseup = event_attrib "onmouseup"
  let a_onmouseover = event_attrib "onmouseover"
  let a_onmousemove = event_attrib "onmousemove"
  let a_onmouseout = event_attrib "onmouseout"
  let a_onkeypress = event_attrib "onkeypress"
  let a_onkeydown = event_attrib "onkeydown"
  let a_onkeyup = event_attrib "onkeyup"


      (* Other Attributes *)

  let a_profile = uri_attrib "profile"
  let a_version = string_attrib "version"
  let a_xmlns = function
    | `W3_org_1999_xhtml -> string_attrib "xmlns" "http://www.w3.org/1999/xhtml"

  let a_cite = uri_attrib "cite"
  let a_xml_space = function
    | `Preserve -> string_attrib "xml:space" "preserve"

  let a_accesskey c = string_attrib "accesskey" (String.make 1 c)
  let a_charset = string_attrib "charset"
  let a_accept_charset = string_attrib "accept-charset"
  let a_accept = string_attrib "accept"
  let a_href = uri_attrib "href"
  let a_hreflang = string_attrib "hreflang"
  let a_rel = linktypes_attrib "rel"
  let a_rev = linktypes_attrib "rev"
  let a_tabindex = int_attrib "tabindex"
  let a_type = string_attrib "type"

  let a_alt = string_attrib "alt"
  let a_height p = length_attrib "height" p
  let a_longdesc = uri_attrib "longdesc"
  let a_src = uri_attrib "src"
  let a_width p = length_attrib "width" p

  let a_for = string_attrib "for"
  let a_selected = function
    | `Selected -> string_attrib "selected" "selected"
  let a_value = string_attrib "value"
  let a_action = uri_attrib "action"
  let a_method m =
    string_attrib "method" (match m with `Get ->  "get" | `Post -> "post")
  let a_enctype = string_attrib "enctype"

  let a_ismap `Ismap = string_attrib "ismap" "ismap"

  let a_checked `Checked = string_attrib "checked" "checked"
  let a_disabled `Disabled = string_attrib "disabled" "disabled"
  let a_readonly `Readonly = string_attrib "readonly" "readonly"
  let a_maxlength = int_attrib "maxlength"
  let a_name = string_attrib "name"

  let a_span = int_attrib "span"

  let a_value_type it =
    string_attrib "valuetype"
      (match it with
      | `Data -> "data"
      | `Ref -> "ref"
      | `Object -> "object")

(* XHTML 1.0 allows the name attribute for more elements:*)
  let a_name_01_00 = string_attrib "name"

  let a_size = int_attrib "size"
  let a_input_type it =
    string_attrib "type"
      (match it with
      | `Text -> "text"
      | `Password -> "password"
      | `Checkbox -> "checkbox"
      | `Radio -> "radio"
      | `Submit -> "submit"
      | `Reset -> "reset"
      | `File -> "file"
      | `Image -> "image"
      | `Button -> "button"
      | `Hidden -> "hidden")
  let a_button_type bt =
    string_attrib "type"
      (match bt with
      | `Button -> "button"
      | `Submit -> "submit"
      | `Reset -> "reset")
  let a_multiple = function
    | `Multiple -> string_attrib "multiple" "multiple"
  let a_cols = int_attrib "cols"
  let a_rows = int_attrib "rows"

  let a_summary = string_attrib "summary"

  let a_abbr = string_attrib "attrib"
  let a_align a =
    string_attrib "align"
      (match a with
      | `Left -> "left"
      | `Center -> "center"
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
  let a_valign v =
    string_attrib "valign"
      (match v with
      | `Top -> "top"
      | `Middle -> "middle"
      | `Bottom -> "bottom"
      | `Baseline -> "baseline")

  let a_border = int_attrib "border"
  let a_cellpadding = length_attrib "cellpadding"
  let a_cellspacing = length_attrib "cellspacing"
  let a_datapagesize = string_attrib "datapagesize"
  let a_frame f =
    string_attrib "frame"
      (match f with
      | `Void -> "void"
      | `Above -> "above"
      | `Below -> "below"
      | `Hsides -> "hsides"
      | `LHS -> "lhs"
      | `RHS -> "rhs"
      | `Vsides -> "vsides"
      | `Box -> "box"
      | `Border -> "border")
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

  let a_declare `Declare = string_attrib "declare" "declare"
  let a_classid = uri_attrib "classid"
  let a_codebase = uri_attrib "codebase"
  let a_data = uri_attrib "data"
  let a_codetype = string_attrib "codetype"
  let a_archive = uri_attrib "archive"
  let a_standby = string_attrib "standby"

  let a_fs_rows mls = multilengths_attrib "rows" mls
  let a_fs_cols mls = multilengths_attrib "cols" mls
  let a_frameborder b =
    int_attrib "frameborder" (match b with `Zero -> 0 | `One -> 1)
  let a_marginheight = int_attrib "marginheight"
  let a_marginwidth = int_attrib "marginwidth"
  let a_noresize `Noresize = string_attrib "noresize" "noresize"
  let a_scrolling s =
    string_attrib "scrolling"
      (match s with
      | `Yes -> "yes"
      | `No -> "no"
      | `Auto -> "auto")

  let a_target = string_attrib "target"

  let a_content = string_attrib "content"
  let a_http_equiv = string_attrib "http-equiv"
  let a_scheme = string_attrib "scheme"

  let a_media = mediadesc_attrib "media"

  type 'a elt = XML.elt

  type html = [`Html] elt

	(* NB: These are more general than the ones in xHTML.mli *)

  type ('a, 'b) nullary = ?a:('a attrib list) -> unit -> 'b elt
  type ('a, 'b, 'c) unary = ?a:('a attrib list) -> 'b elt -> 'c elt
  type ('a, 'b, 'c, 'd) binary = ?a:('a attrib list) -> 'b elt -> 'c elt -> 'd elt
(* CH *)
  type ('a, 'b, 'c, 'd, 'e, 'f) quadry= ?a:('a attrib list) -> 'b elt -> 'c elt -> 'd elt -> 'e elt -> 'f elt
(* CH *)
  type ('a, 'b, 'c) star = ?a:('a attrib list) -> 'b elt list -> 'c elt
  type ('a, 'b, 'c) plus = ?a:('a attrib list) -> 'b elt -> 'b elt list -> 'c elt

  let terminal tag ?a () = XML.leaf ?a tag
      (* let nullary tag ?a () = XML.node ?a tag [] *)
  let unary tag ?a elt = XML.node ?a tag [elt]
  let binary tag ?a elt1 elt2 = XML.node ?a tag [elt1; elt2]
  let star tag ?a elts = XML.node ?a tag elts
  let plus tag ?a elt elts = XML.node ?a tag (elt :: elts)

(* CH *)
  let quadry tag ?a elt1 elt2 elt3 elt4 = XML.node ?a tag [elt1; elt2; elt3; elt4]
(* CH *)


  module STRUCTURE =
    struct
      type t = [ `Body | `Head | `Html | `Title ]
    end

  let body = star "body"
  let head = plus "head"
  let title = unary "title"
  let html = binary "html"

  let pcdata = XML.pcdata
  let entity = XML.entity

  let space () = entity "nbsp"

  let cdata = XML.cdata

  let cdata_script = XML.cdata_script

  let cdata_style = XML.cdata_style

  let unsafe_data s = XML.encodedpcdata s


  module TEXT =
    struct
      type heading = [ `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ]
      type block = [ `Address | `Blockquote | `Div | `P | `Pre ]
      type inline =
          [ `Abbr | `Acronym | `Br | `Cite | `Code | `Dfn
        | `Em | `Kbd | `Q | `Samp | `Span | `Strong | `Var ]
      type flow = [ heading | block | inline ]
    end

  let h1 = star "h1"
  let h2 = star "h2"
  let h3 = star "h3"
  let h4 = star "h4"
  let h5 = star "h5"
  let h6 = star "h6"

  let address = star "address"
  let blockquote = star "blockquote"
  let div = star "div"
  let p = star "p"
  let pre = star "pre"

  let abbr = star "abbr"
  let acronym = star "acronym"
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
  let var = star "var"

  module HYPERTEXT =
    struct
      type inline = [ `A ]
      type flow = inline
    end

  let a = star "a"

  module LIST =
    struct
      type list = [ `Dl | `Ol | `Ul ]
      type t = [ `Dd | `Dt | `Li ]
      type flow = list
    end

  let dl = plus "dl"
  let ol = plus "ol"
  let ul = plus "ul"
  let dd = star "dd"
  let dt = star "dt"
  let li = star "li"

  module PRESENTATION =
    struct
      type block = [ `Hr ]
      type inline = [ `B | `Big | `I | `Small | `Sub | `Sup | `Tt ]
      type flow = [ inline | block ]
    end

  let hr = terminal "hr"
  let b = star "b"
  let big = star "big"
  let i = star "i"
  let small = star "small"
  let sub = star "sub"
  let sup = star "sup"
  let tt = star "tt"


(* VB *)
  type i18nclass = [ `Bdo ]
  type shape = [ `Rect | `Circle | `Poly | `Default ]


  let a_datetime = string_attrib "datetime"

  let a_dir d =
    string_attrib "dir" (match d with `Rtl -> "rtl" | `Ltr -> "ltr")

(* CH *)
  let bdo ~dir ?(a = []) elts =
    XML.node ~a:(a_dir dir :: a) "bdo" elts
(* CH *)

  let a_shape d =
    string_attrib "shape"
      (match d with
        `Rect -> "rect"
      | `Circle -> "circle"
      | `Poly -> "poly"
      | `Default -> "default")
  let a_coords coords = 
    string_attrib "coords" (String.concat ","
                              (List.map string_of_int coords))

  let a_nohref `Nohref = string_attrib "nohref" "nohref"
  let a_usemap = string_attrib "usemap"
  let a_defer `Defer = string_attrib "defer" "defer"
  let a_label = string_attrib "label"

  let area ~alt ?(a = []) () =
    XML.leaf ~a:(a_alt alt :: a) "area"
  let map ~id ?(a = []) elt elts =
    XML.node ~a:(a_id id :: a) "map" (elt::elts)
  let del = star "del"
  let ins = star "ins"
  let script ~contenttype ?(a = []) elt =
    XML.node ~a:(a_type contenttype :: a) "script" [elt]
  let noscript = plus "noscript"
(* VB *)

  module FORMS =
    struct
      type t = [ `Option ]
      type form = [ `Form ]
      type formctrl_sans_label = [ `Input | `Select | `Textarea | `Button ]
      type formctrl = [ `Label | formctrl_sans_label ]
      type block = form
      type inline_sans_label = formctrl_sans_label
      type inline = formctrl
      type flow_sans_label = [block | inline_sans_label ]
      type flow = [ block | inline ]
    end

  module Basic_Forms =
    struct
      let form ~action ?(a = []) elt elts =
        XML.node ~a:(a_action action :: a) "form" (elt::elts)
      let input = terminal "input"
      let label = star "label"
      let option = unary "option"
      let select = plus "select"
      let textarea ~rows ~cols ?(a = []) elt =
        XML.node ~a:(a_rows rows :: a_cols cols :: a) "textarea" [elt]
    end

  let form ~action ?(a = []) elt elts =
    XML.node ~a:(a_action action :: a) "form" (elt::elts)
  let input = terminal "input"
  let label = star "label"
  let option = unary "option"
  let select = plus "select"
  let textarea ~rows ~cols ?(a = []) elt =
    XML.node ~a:(a_rows rows :: a_cols cols :: a) "textarea" [elt]
  let button = star "button"
  let legend = star "legend"
  let fieldset = star "fieldset"
  let optgroup ~label ?(a = []) elt elts =
    XML.node ~a:(a_label label :: a) "optgroup" (elt :: elts)

  module TABLES =
    struct
      type t = [ `Caption | `Td | `Th | `Tr ]
      type block = [ `Table ]
      type flow = block
    end

  let list_of_option = function
    | Some x -> [x]
    | None -> []

  let list_of_list_option = function
    | Some x -> x
    | None -> []

  module Basic_Tables =
    struct
      let a_align = a_align
      let a_scope = a_scope
      let a_valign = a_valign
      let caption = star "caption"
      let table ?caption ?a elt elts =
        XML.node ?a "table" (list_of_option caption @ elt :: elts)
      let td = star "td"
      let th = star "th"
      let tr = plus "tr"
    end

  let caption = star "caption"

  let cols_option = function
    | Some (`Cols c) -> c
    | Some (`Colgroups c) -> c
    | None -> []

  let table ?caption ?columns ?a elt elts =
    XML.node ?a "table"
      (list_of_option caption @ cols_option columns @ elt :: elts)

  let tablex ?caption ?columns ?thead ?tfoot ?a elt elts =
    XML.node ?a "table"
      (list_of_option caption @ cols_option columns @
       list_of_option thead @ list_of_option tfoot @ elt :: elts)

  let td = star "td"
  let th = star "th"
  let tr = plus "tr"

  let col = terminal "col"
  let colgroup = star "colgroup"
  let thead = plus "thead"
  let tbody = plus "tbody"
  let tfoot = plus "tfoot"

  let object_ = star "object"
  let param ~name ?(a = []) () =
    XML.leaf ~a:(a_name name :: a) "param"

  let img ~src ~alt ?(a = []) () =
    XML.leaf ~a:(a_src src :: a_alt alt :: a) "img"

  let frameset ?noframes ?a elt elts =
    XML.node ?a "frameset"
      (elt :: elts @ (match noframes with None -> [] | Some e -> [e]))
  let frame ~src ?(a = []) () =
    XML.leaf ~a:(a_src src :: a) "frame"
  let noframes = unary "noframes"
  let iframe = star "iframe"

  module METAINFORMATION =
    struct
      type t = [ `Meta ]
    end

  let meta ~content ?(a = []) () =
    XML.leaf ~a:(a_content content :: a) "meta"

  module STYLE_SHEET =
    struct
      type t = [ `Style ]
    end

  let style ~contenttype ?(a = []) elts =
    XML.node ~a:(a_type contenttype :: a) "style" elts

  module LINK =
    struct
      type t = [ `Link ]
    end

  let link = terminal "link"

  module BASE =
    struct
      type t = [ `Base ]
    end

  let base ~href ?(a = []) ()=
    XML.leaf ~a:(a_href href :: a) "base"

  let ruby_simple1 = binary "ruby"
  let ruby_simple2 = quadry "ruby"
  let ruby_complex = binary "ruby"

  let rbc = plus "rbc"
  let rtc = plus "rtc"
  let rtc_complex= plus "rtc"
  let rb = star "rb"
  let rt = star "rt"
  let rt_complex = star "rt"
  let rp = star "rp"

  let a_rbspan = int_attrib "rbspan"


(* VB *)
  type edit = [ `Ins | `Del ]
  type scripttag = [ `Script | `Noscript ]
  type misc = [ edit | scripttag ]

  module SPECIAL = struct
    type inline = [ `Img | `Map | `Object ]
    type block = [ `Table | `Form | `Fieldset ]
    type flow = [ inline | block ]
  end

(* VB *)

(* CH *)
  module RUBY = struct
    type inline = [ `Ruby_simple1 | `Ruby_simple2 | `Ruby_complex ]
    type flow =  inline 
  end

  type no_ruby_inline = [ TEXT.inline | PRESENTATION.inline | HYPERTEXT.inline | SPECIAL.inline | FORMS.inline | i18nclass ]
  type no_ruby_content = [ `PCDATA | no_ruby_inline | misc ]
(* CH *)

  type block =
      [ TEXT.block | PRESENTATION.block | FORMS.block | TABLES.block | SPECIAL.block | TEXT.heading | LIST.list | misc ]
  type block_sans_form =
      [ TEXT.block | PRESENTATION.block | TABLES.block | TEXT.heading | LIST.list | misc ]

  type flow =
      [ TEXT.flow | HYPERTEXT.flow | LIST.flow | FORMS.flow | TABLES.flow | PRESENTATION.flow | SPECIAL.flow | i18nclass | misc | RUBY.flow ]
  type flow_sans_table =
      [ TEXT.flow | HYPERTEXT.flow | LIST.flow | FORMS.flow | PRESENTATION.flow | SPECIAL.flow | i18nclass | misc | RUBY.flow ]

  type inline =
      [ TEXT.inline | HYPERTEXT.inline | PRESENTATION.inline
    | FORMS.inline | SPECIAL.inline | i18nclass | misc | RUBY.inline ]

  type inline_sans_a_mix =
      [ TEXT.inline | PRESENTATION.inline
    | FORMS.inline | SPECIAL.inline | i18nclass | misc | RUBY.inline ]

  type buttoncontent = (* VB *)
      [ TEXT.inline | PRESENTATION.inline
    | SPECIAL.inline | i18nclass | block_sans_form ]
  type precontent = inline
  type inline_sans_label =
      [ TEXT.inline | HYPERTEXT.inline | PRESENTATION.inline
    | FORMS.inline_sans_label | SPECIAL.inline | i18nclass | misc ]

  type heading = TEXT.heading

(*
  let validator =
  "http://validator.w3.org/check/referer"

  let compose_validator_icon icon alt =
  a ~a:[a_href validator]
  [img ~src:icon ~alt ~a:[a_height (`Pixels 31); a_width (`Pixels 88)] ()]

  let validator_icon = function
  | `XHTML_01_00 -> compose_validator_icon
  "http://www.w3.org/Icons/valid-xhtml10" "Valid XHTML 1.0!"
  | `XHTML_01_01 -> compose_validator_icon
  "http://www.w3.org/Icons/valid-xhtml11" "Valid XHTML 1.1!"
 *)

  (******************************************************************)
  (* In the following, my own stuffs for Ocsigen -- Vincent: *)

  let tot x = x
  let totl x = x
  let toelt x = x
  let toeltl x = x

  type doc = [ `Html ] elt
  let doc_toelt x = x

end

(* The following tags are written <br />, etc.
   The other empty tags are written <p></p> for html compatibility.
   See guidelines here:
   http://www.w3.org/TR/xhtml1/#guidelines
 *)
let emptytags =
  [ "hr"; "br"; "img"; "meta"; "link"; "input"; "col"; "area";
    "param"; "base"; "basefont"; "isindex"; "frame" ]

module Make_01_00(XML : XML_sigs.T) = struct

  module M = Version(XML)
  include M

  module Info = struct
    let content_type = "text/html"
    let alternative_content_types = ["application/xhtml+xml"]
    let version = "XHTML 1.0"
    let standard = Uri.uri_of_string "http://www.w3.org/TR/xhtml1/"
    let namespace = "http://www.w3.org/1999/xhtml"
    let doctype =
      XML_print.compose_doctype	"html"
	["-//W3C//DTD XHTML 1.0 Strict//EN";
	 "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"]
    let emptytags = emptytags
  end

end

module Make_01_01(XML : XML_sigs.T) = struct

  module M = Version(XML)
  include M

  module Info = struct
    let content_type = "text/html"
    let alternative_content_types = ["application/xhtml+xml"]
    let version = "XHTML 1.1"
    let standard = Uri.uri_of_string "http://www.w3.org/TR/xhtml11/"
    let namespace = "http://www.w3.org/1999/xhtml"
    let doctype =
      XML_print.compose_decl () ^
      XML_print.compose_doctype "html"
	["-//W3C//DTD XHTML 1.1//EN";
	 "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"]
    let emptytags = emptytags
  end

end

module Make = Make_01_01
