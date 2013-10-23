(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2011 Pierre Chambart, Grégoire Henry
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

module type T = sig

  open Html5_types

  module Xml : Xml_sigs.T
  module Svg : Svg_sigs.T with module Xml := Xml
  module Info : Xml_sigs.Info

  type uri = Xml.uri
  val string_of_uri : uri -> string
  val uri_of_string : string -> uri

  (** {1 Common Attributes} *)

  type +'a attrib

  val to_xmlattribs : ('a attrib) list -> Xml.attrib list (* VB *)
  val to_attrib : Xml.attrib -> 'a attrib (* GH *)
  (** ['a] is known as a {i phantom type}.  The implementation is
      actually monomorphic (the different element types are distinguished
      by a homogeneous variable, such as their textual representation)
      and the type variable [`a] is just used by the type checker.

      NB: It might be possible to use polymorphic variants directly, without
      phantom types, but the implementation is likely to be more involved. *)

  val a_autocomplete : [< | `On | `Off] -> [> | `Autocomplete] attrib

  val a_async : [< | `Async] -> [> | `Async] attrib

  val a_autofocus : [< | `Autofocus] -> [> | `Autofocus] attrib

  val a_autoplay : [< | `Autoplay] -> [> | `Autoplay] attrib

  val a_challenge : text -> [> | `Challenge] attrib

  val a_contenteditable :
    [< | `True | `False] -> [> | `Contenteditable] attrib

  val a_contextmenu : idref -> [> | `Contextmenu] attrib

  val a_controls : [< | `Controls] -> [> | `Controls] attrib

  val a_dir : [< | `Rtl | `Ltr] -> [> | `Dir] attrib

  val a_draggable : [< | `True | `False] -> [> | `Draggable] attrib

  val a_form : idref -> [> | `Form] attrib

  val a_formaction : Xml.uri -> [> | `Formaction] attrib

  val a_formenctype : contenttype -> [> | `Formenctype] attrib

  val a_formmethod :
    [< | `Get | `Post | `Put | `Delete] -> [> | `Formmethod] attrib

  val a_formnovalidate :
    [< | `Formnovalidate] -> [> | `Formnovalidate] attrib

  val a_formtarget : text -> [> | `Formtarget] attrib

  val a_hidden : [< | `Hidden] -> [> | `Hidden] attrib

  val a_high : float_number -> [> | `High] attrib

  val a_icon : Xml.uri -> [> | `Icon] attrib

  val a_ismap : [< | `Ismap] -> [> | `Ismap] attrib

  val a_keytype : text -> [> | `Keytype] attrib

  val a_list : idref -> [> | `List] attrib

  val a_loop : [< | `Loop] -> [> | `Loop] attrib

  val a_low : float_number -> [> | `High] attrib

  val a_max : float_number -> [> | `Max] attrib

  val a_input_max : float_number -> [> | `Input_Max] attrib

  val a_min : float_number -> [> | `Min] attrib

  val a_input_min : float_number -> [> | `Input_Min] attrib

  val a_novalidate : [< | `Novalidate] -> [> | `Novalidate] attrib

  val a_open : [< | `Open] -> [> | `Open] attrib

  val a_optimum : float_number -> [> | `Optimum] attrib

  val a_pattern : text -> [> | `Pattern] attrib

  val a_placeholder : text -> [> | `Placeholder] attrib

  val a_poster : Xml.uri -> [> | `Poster] attrib

  val a_preload : [< | `None | `Metadata | `Audio] -> [> | `Preload] attrib

  val a_pubdate : [< | `Pubdate] -> [> | `Pubdate] attrib

  val a_radiogroup : text -> [> | `Radiogroup] attrib

  val a_required : [< | `Required] -> [> | `Required] attrib

  val a_reversed : [< | `Reversed] -> [> | `Reversed] attrib

  val a_sandbox :
    [< | `AllowSameOrigin | `AllowForms | `AllowScript] list ->
    [> | `Sandbox] attrib

  val a_spellcheck : [< | `True | `False] -> [> | `Spellcheck] attrib

  val a_scoped : [< | `Scoped] -> [> | `Scoped] attrib

  val a_seamless : [< | `Seamless] -> [> | `Seamless] attrib

  val a_sizes : numbers -> [> | `Sizes] attrib

  val a_span : number -> [> | `Span] attrib

  (*val a_srcdoc*)
  val a_srclang : nmtoken -> [> | `XML_lang] attrib

  val a_start : number -> [> | `Start] attrib

  val a_step : float_number option -> [> | `Step] attrib

  val a_wrap : [< | `Soft | `Hard] -> [> | `Wrap] attrib

  val a_class : nmtokens -> [> | `Class] attrib
  (** This attribute assigns a class name or set of class names to an
      element. Any number of elements may be assigned the same class
      name or names.  *)

  val a_user_data : nmtoken -> text -> [> | `User_data] attrib
  (** May be used to specify custom attribs.
      The example given by the W3C is as follows :
      {v <ol>
      <li data-length="2m11s">Beyond The Sea</li>
      </ol> v}
      It should be used for preprocessing ends only. *)
  val a_id : text -> [> | `Id] attrib
  (** This attribute assigns a name to an element. This name must be
      unique in a document. The text should be without any space. *)

  val a_title : text -> [> | `Title] attrib
  (** This attribute offers advisory information about the element for
      which it is set. *)

  (** Values of the title attribute may be rendered by user agents in a
      variety of ways. For instance, visual browsers frequently display
      the title as a {i tool tip} (a short message that appears when the
      pointing device pauses over an object). Audio user agents may
      speak the title information in a similar context.  *)

  (** The title attribute has an additional role when used with the [link]
      element to designate an external style sheet. Please consult the
      section on links and style sheets for details.  *)

  (** {2 I18N} *)

  val a_xml_lang : nmtoken -> [> | `XML_lang] attrib

  (** {2 Events} *)

  (** Javascript events *)
  val a_onabort : Xml.event_handler -> [> | `OnAbort] attrib

  val a_onafterprint : Xml.event_handler -> [> | `OnAfterPrint] attrib

  val a_onbeforeprint : Xml.event_handler -> [> | `OnBeforePrint] attrib

  val a_onbeforeunload : Xml.event_handler -> [> | `OnBeforeUnload] attrib

  val a_onblur : Xml.event_handler -> [> | `OnBlur] attrib

  val a_oncanplay : Xml.event_handler -> [> | `OnCanPlay] attrib

  val a_oncanplaythrough : Xml.event_handler -> [> | `OnCanPlayThrough] attrib

  val a_onchange : Xml.event_handler -> [> | `OnChange] attrib

  val a_onclick : Xml.event_handler -> [> | `OnClick] attrib

  val a_oncontextmenu : Xml.event_handler -> [> | `OnContextMenu] attrib

  val a_ondblclick : Xml.event_handler -> [> | `OnDblClick] attrib

  val a_ondrag : Xml.event_handler -> [> | `OnDrag] attrib

  val a_ondragend : Xml.event_handler -> [> | `OnDragEnd] attrib

  val a_ondragenter : Xml.event_handler -> [> | `OnDragEnter] attrib

  val a_ondragleave : Xml.event_handler -> [> | `OnDragLeave] attrib

  val a_ondragover : Xml.event_handler -> [> | `OnDragOver] attrib

  val a_ondragstart : Xml.event_handler -> [> | `OnDragStart] attrib

  val a_ondrop : Xml.event_handler -> [> | `OnDrop] attrib

  val a_ondurationchange : Xml.event_handler -> [> | `OnDurationChange] attrib

  val a_onemptied : Xml.event_handler -> [> | `OnEmptied] attrib

  val a_onended : Xml.event_handler -> [> | `OnEnded] attrib

  val a_onerror : Xml.event_handler -> [> | `OnError] attrib

  val a_onfocus : Xml.event_handler -> [> | `OnFocus] attrib

  val a_onformchange : Xml.event_handler -> [> | `OnFormChange] attrib

  val a_onforminput : Xml.event_handler -> [> | `OnFormInput] attrib

  val a_onhashchange : Xml.event_handler -> [> | `OnHashChange] attrib

  val a_oninput : Xml.event_handler -> [> | `OnInput] attrib

  val a_oninvalid : Xml.event_handler -> [> | `OnInvalid] attrib

  val a_onmousedown : Xml.event_handler -> [> | `OnMouseDown] attrib

  val a_onmouseup : Xml.event_handler -> [> | `OnMouseUp] attrib

  val a_onmouseover : Xml.event_handler -> [> | `OnMouseOver] attrib

  val a_onmousemove : Xml.event_handler -> [> | `OnMouseMove] attrib

  val a_onmouseout : Xml.event_handler -> [> | `OnMouseOut] attrib

  val a_onmousewheel : Xml.event_handler -> [> | `OnMouseWheel] attrib

  val a_onoffline : Xml.event_handler -> [> | `OnOffLine] attrib

  val a_ononline : Xml.event_handler -> [> | `OnOnLine] attrib

  val a_onpause : Xml.event_handler -> [> | `OnPause] attrib

  val a_onplay : Xml.event_handler -> [> | `OnPlay] attrib

  val a_onplaying : Xml.event_handler -> [> | `OnPlaying] attrib

  val a_onpagehide : Xml.event_handler -> [> | `OnPageHide] attrib

  val a_onpageshow : Xml.event_handler -> [> | `OnPageShow] attrib

  val a_onpopstate : Xml.event_handler -> [> | `OnPopState] attrib

  val a_onprogress : Xml.event_handler -> [> | `OnProgress] attrib

  val a_onratechange : Xml.event_handler -> [> | `OnRateChange] attrib

  val a_onreadystatechange : Xml.event_handler -> [> | `OnReadyStateChange] attrib

  val a_onredo : Xml.event_handler -> [> | `OnRedo] attrib

  val a_onresize : Xml.event_handler -> [> | `OnResize] attrib

  val a_onscroll : Xml.event_handler -> [> | `OnScroll] attrib

  val a_onseeked : Xml.event_handler -> [> | `OnSeeked] attrib

  val a_onseeking : Xml.event_handler -> [> | `OnSeeking] attrib

  val a_onselect : Xml.event_handler -> [> | `OnSelect] attrib

  val a_onshow : Xml.event_handler -> [> | `OnShow] attrib

  val a_onstalled : Xml.event_handler -> [> | `OnStalled] attrib

  val a_onstorage : Xml.event_handler -> [> | `OnStorage] attrib

  val a_onsubmit : Xml.event_handler -> [> | `OnSubmit] attrib

  val a_onsuspend : Xml.event_handler -> [> | `OnSuspend] attrib

  val a_ontimeupdate : Xml.event_handler -> [> | `OnTimeUpdate] attrib

  val a_onundo : Xml.event_handler -> [> | `OnUndo] attrib

  val a_onunload : Xml.event_handler -> [> | `OnUnload] attrib

  val a_onvolumechange : Xml.event_handler -> [> | `OnVolumeChange] attrib

  val a_onwaiting : Xml.event_handler -> [> | `OnWaiting] attrib

  val a_onkeypress : Xml.event_handler -> [> | `OnKeyPress] attrib

  val a_onkeydown : Xml.event_handler -> [> | `OnKeyDown] attrib

  val a_onkeyup : Xml.event_handler -> [> | `OnKeyUp] attrib

  val a_onload : Xml.event_handler -> [> | `OnLoad] attrib

  val a_onloadeddata : Xml.event_handler -> [> | `OnLoadedData] attrib

  val a_onloadedmetadata : Xml.event_handler -> [> | `OnLoadedMetaData] attrib

  val a_onloadstart : Xml.event_handler -> [> | `OnLoadStart] attrib

  val a_onmessage : Xml.event_handler -> [> | `OnMessage] attrib

  val a_version : cdata -> [> | `Version] attrib

  val a_xmlns : [< | `W3_org_1999_xhtml] -> [> | `XMLns] attrib

  val a_manifest : Xml.uri -> [> | `Manifest] attrib

  val a_cite : Xml.uri -> [> | `Cite] attrib

  val a_xml_space : [< | `Preserve] -> [> | `XML_space] attrib

  val a_accesskey : character -> [> | `Accesskey] attrib
  (** This attribute assigns an access key to an element. An access key
      is a single character from the document character
      set. NB: authors should consider the input method of the
      expected reader when specifying an accesskey. *)

  val a_charset : charset -> [> | `Charset] attrib
  (** This attribute specifies the character encoding of the resource
      designated by the link. Please consult the section on character
      encodings for more details. *)

  val a_accept_charset : charsets -> [> | `Accept_charset] attrib

  val a_accept : contenttypes -> [> | `Accept] attrib

  val a_href : Xml.uri -> [> | `Href] attrib
  (** This attribute specifies the location of a Web resource, thus
      defining a link between the current element (the source anchor)
      and the destination anchor defined by this attribute. *)

  val a_hreflang : languagecode -> [> | `Hreflang] attrib
  (** This attribute specifies the base language of the resource
      designated by href and may only be used when href is specified. *)

  val a_rel : linktypes -> [> | `Rel] attrib
  (** This attribute describes the relationship from the current
      document to the anchor specified by the href attribute. The
      value of this attribute is a space-separated list of link
      types. *)

  (** This attribute is used to describe a reverse link from the
      anchor specified by the href attribute to the current
      document. The value of this attribute is a space-separated
      list of link types. *)

  val a_tabindex : number -> [> | `Tabindex] attrib
  (** This attribute specifies the position of the current
      element in the tabbing order for the current document. This
      value must be a number between 0 and 32767. User agents
      should ignore leading zeros. *)

  val a_mime_type : contenttype -> [> | `Mime_type] attrib
  (** This attribute gives an advisory hint as to the content type
      of the content available at the link target address. It
      allows user agents to opt to use a fallback mechanism rather
      than fetch the content if they are advised that they will
      get content in a content type they do not support.Authors
      who use this attribute take responsibility to manage the
      risk that it may become inconsistent with the content
      available at the link target address. *)

  val a_datetime : cdata -> [> | `Datetime] attrib

  val a_action : Xml.uri -> [> | `Action] attrib
  (** This attribute specifies a form processing agent. User agent
      behavior for a value other than an HTTP URI is undefined. *)

  val a_checked : [< | `Checked] -> [> | `Checked] attrib
  (** When the [type] attribute has the value ["radio"] or
      ["checkbox"], this boolean attribute specifies that the
      button is on. User agents must ignore this attribute for
      other control types. *)

  val a_cols : number -> [> | `Cols] attrib
  (** This attribute specifies the visible width in average
      character widths. Users should be able to enter longer lines
      than this, so user agents should provide some means to
      scroll through the contents of the control when the contents
      extend beyond the visible area. User agents may wrap visible
      text lines to keep long lines visible without the need for
      scrolling. *)

  val a_enctype : contenttype -> [> | `Enctype] attrib

  val a_for : idref -> [> | `For] attrib

  val a_for_list : idrefs -> [> | `For_List] attrib

  val a_maxlength : number -> [> | `Maxlength] attrib

  val a_method :
    [< | `Get | `Post | `Put | `Delete] -> [> | `Method] attrib

  val a_multiple : [< | `Multiple] -> [> | `Multiple] attrib

  val a_name : text -> [> | `Name] attrib
  (** This attribute assigns the control name. *)

  val a_rows : number -> [> | `Rows] attrib
  (** This attribute specifies the number of visible text
      lines. Users should be able to enter more lines than this,
      so user agents should provide some means to scroll through
      the contents of the control when the contents extend beyond
      the visible area. *)

  val a_selected : [< | `Selected] -> [> | `Selected] attrib
  (** When set, this boolean attribute specifies that
      this option is pre-selected. *)

  val a_size : number -> [> | `Size] attrib

  val a_src : Xml.uri -> [> | `Src] attrib

  val a_input_type : [<
		     | `Url
		     | `Tel
		     | `Text
		     | `Time
		     | `Search
		     | `Password
		     | `Checkbox
		     | `Range
		     | `Radio
		     | `Submit
		     | `Reset
		     | `Number
		     | `Hidden
		     | `Month
		     | `Week
		     | `File
		     | `Email
		     | `Image
		     | `Datetime_local
		     | `Datetime
		     | `Date
		     | `Color
		     | `Button] -> [> | `Input_Type] attrib

  val a_text_value : text -> [> | `Text_Value] attrib
  (** This attribute specifies the initial value of the
      control. If this attribute is not set, the initial value is
      set to the contents of the [option] element. *)

  val a_int_value : number -> [> | `Int_Value] attrib

  (*VVV NO *)
  val a_value : cdata -> [> | `Value] attrib

  val a_float_value : float_number -> [> | `Float_Value] attrib

  val a_disabled : [< | `Disabled] -> [> | `Disabled] attrib

  val a_readonly : [< | `ReadOnly] -> [> | `ReadOnly] attrib
  val a_button_type :
    [< | `Button | `Submit | `Reset] -> [> | `Button_Type] attrib

  val a_command_type :
    [< | `Command | `Checkbox | `Radio] -> [> | `Command_Type] attrib

  val a_menu_type : [< | `Context | `Toolbar] -> [> | `Menu_Type] attrib

  val a_label : text -> [> | `Label] attrib

  val a_align :
    [< | `Left | `Right | `Justify | `Char] -> [> | `Align] attrib

  val a_axis : cdata -> [> | `Axis] attrib

  val a_colspan : number -> [> | `Colspan] attrib

  val a_headers : idrefs -> [> | `Headers] attrib

  val a_rowspan : number -> [> | `Rowspan] attrib

  val a_scope :
    [< | `Row | `Col | `Rowgroup | `Colgroup] -> [> | `Scope] attrib

  val a_summary : text -> [> | `Summary] attrib

  val a_border : pixels -> [> | `Border] attrib

  val a_cellpadding : length -> [> | `Cellpadding] attrib

  val a_cellspacing : length -> [> | `Cellspacing] attrib

  val a_datapagesize : cdata -> [> | `Datapagesize] attrib

  val a_rules :
    [< | `None | `Groups | `Rows | `Cols | `All] -> [> | `Rules] attrib

  val a_char : character -> [> | `Char] attrib

  val a_charoff : length -> [> | `Charoff] attrib

  val a_alt : text -> [> | `Alt] attrib

  val a_height : number -> [> | `Height] attrib

  val a_width : number -> [> | `Width] attrib

  type shape = [ | `Rect | `Circle | `Poly | `Default ]

  val a_shape : shape -> [> | `Shape] attrib

  val a_coords : numbers -> [> | `Coords] attrib

  val a_usemap : idref -> [> | `Usemap] attrib

  val a_data : Xml.uri -> [> | `Data] attrib

  val a_codetype : contenttype -> [> | `Codetype] attrib

  val a_fs_rows : multilengths -> [> | `FS_Rows] attrib

  val a_fs_cols : multilengths -> [> | `FS_Cols] attrib

  val a_frameborder : [< | `Zero | `One] -> [> | `Frameborder] attrib

  val a_marginheight : pixels -> [> | `Marginheight] attrib

  val a_marginwidth : pixels -> [> | `Marginwidth] attrib

  val a_scrolling : [< | `Yes | `No | `Auto] -> [> | `Scrolling] attrib

  val a_target : frametarget -> [> | `Target] attrib

  val a_content : text -> [> | `Content] attrib

  val a_http_equiv : text -> [> | `Http_equiv] attrib

  val a_defer : [< | `Defer] -> [> | `Defer] attrib

  val a_media : mediadesc -> [> | `Media] attrib

  val a_style : string -> [> | `Style_Attr] attrib

  val a_property : string -> [> | `Property] attrib

  (** {1 Phantom types and XML elements} *)

  (* For Ocsigen I need to specify the variance --Vincent *)
  type +'a elt

  type ('a, 'b) nullary = ?a: (('a attrib) list) -> unit -> 'b elt

  type ('a, 'b, 'c) unary = ?a: (('a attrib) list) -> 'b elt -> 'c elt

  type ('a, 'b, 'c, 'd) binary =
      ?a: (('a attrib) list) -> 'b elt -> 'c elt -> 'd elt

  type ('b, 'c, 'd, 'e) tri = 'b elt -> 'c elt -> 'd elt -> 'e elt

  type ('a, 'b, 'c) star =
      ?a: (('a attrib) list) -> ('b elt) list -> 'c elt
  (** Star '*' denotes any number of children, uncluding zero. *)

  type ('a, 'b, 'c) plus =
      ?a: (('a attrib) list) -> 'b elt -> ('b elt) list -> 'c elt

  (** Root element *)
  type html = [ | `Html ] elt

  type rt = [
  | `Rt of [ | `Rt ] elt
  | `Rpt of (([ | `Rp ] elt) * ([ | `Rt ] elt) * ([ | `Rp ] elt))
  ]

  type ruby_content = (((phrasing elt) list) * rt)

  type rp = (((common attrib) list) * ((phrasing elt) list))

  (** {1 Combined Element Sets:} *)

  (********************************)
  (*  If the document is an       *)
  (*  iframe srcdoc document or if*)
  (*title information is available*)
  (*from a higher-level protocol: *)
  (*   Zero or more elements of   *)
  (*   metadata content.          *)
  (*Otherwise:                    *)
  (*   One or more elements of    *)
  (*   metadata content, of which *)
  (*exactly one is a title element*)
  (********************************)

  val html :
    ?a: ((html_attrib attrib) list) ->
    [< | `Head] elt -> [< | `Body] elt -> [> | `Html] elt

  val head :
    ?a: ((head_attrib attrib) list) ->
    [< | `Title] elt -> (head_content_fun elt) list -> [> | head] elt

  val base : ([< | base_attrib], [> | base]) nullary

  val title : (title_attrib, [< | title_content_fun], [> | title]) unary

  val body : ([< | body_attrib], [< | body_content_fun], [> | body]) star


  val svg : ?xmlns : string -> ?a : [< svg_attrib ] Svg.attrib list -> [< svg_content ] Svg.elt list -> [> svg ] elt

  (** {2 Section} *)

  val footer :
    ([< | common], [< | flow5_without_header_footer], [> | `Footer]) star

  val header :
    ([< | common], [< | flow5_without_header_footer], [> | `Header]) star

  val section :
    ([< | section_attrib], [< | section_content_fun], [> | section]) star

  val nav : ([< | nav_attrib], [< | nav_content_fun], [> | nav]) star

  val h1 : ([< | h1_attrib], [< | h1_content_fun], [> | h1]) star

  val h2 : ([< | h2_attrib], [< | h2_content_fun], [> | h2]) star

  val h3 : ([< | h3_attrib], [< | h3_content_fun], [> | h3]) star

  val h4 : ([< | h4_attrib], [< | h4_content_fun], [> | h4]) star

  val h5 : ([< | h5_attrib], [< | h5_content_fun], [> | h5]) star

  val h6 : ([< | h6_attrib], [< | h6_content_fun], [> | h6]) star

  val hgroup :
    ([< | hgroup_attrib], [< | hgroup_content_fun], [> | hgroup]) plus

  val address :
    ([< | address_attrib], [< | address_content_fun], [> | address]) star

  val article :
    ([< | article_attrib], [< | article_content_fun], [> | article]) star

  val aside :
    ([< | aside_attrib], [< | aside_content_fun], [> | aside]) star

  (** {2 Grouping content} *)

  val p : ([< | p_attrib], [< | p_content_fun], [> | p]) star

  val pre : ([< | pre_attrib], [< | pre_content_fun], [> | pre]) star

  val blockquote :
    ([< | blockquote_attrib], [< | blockquote_content_fun], [> | blockquote
							    ]) star

  val div : ([< | div_attrib], [< | div_content_fun], [> | div]) star

  (********************************)
  (*            In Dl             *)
  (********************************)
  (*   Zero or more groups each   *)
  (*   consisting of              *)
  (*      one or more dt element  *)
  (*      followed by             *)
  (*      one or more dd  elements*)
  (********************************)
  val dl :
    ?a: (([< | common] attrib) list) ->
    ((([< | `Dt] elt) * (([< | `Dt] elt) list)) *
	(([< | `Dd] elt) * (([< | `Dd] elt) list))) list -> [> | `Dl] elt

  val ol : ([< | ol_attrib], [< | ol_content_fun], [> | ol]) star

  val ul : ([< | ul_attrib], [< | ul_content_fun], [> | ul]) star

  val dd : ([< | dd_attrib], [< | dd_content_fun], [> | dd]) star

  val dt : ([< | dt_attrib], [< | dt_content_fun], [> | dt]) star

  (********************************)
  (*            In Li             *)
  (********************************)
  (*  Only if the element is a    *)
  (*  child of an ol element:     *)
  (*          value attribute     *)
  (********************************)

  (** A list element.
      The 'a type is used to know whether the element has
      a int_value attribute or not. *)
  val li : ([< | li_attrib], [< | li_content_fun], [> | li]) star

  val figcaption :
    ([< | figcaption_attrib], [< | figcaption_content_fun], [> | figcaption]) star

  (********************************)
  (*          In Figure           *)
  (********************************)
  (*Either: One figcaption element*)
  (*     followed by flow content.*)
  (*Or: Flow content followed by  *)
  (*     one figcaption element.  *)
  (*Or: Flow content.             *)
  (********************************)
  val figure :
    ?figcaption:[`Top of [< `Figcaption ] elt | `Bottom of [< `Figcaption ] elt] ->
    ([< | common], [< | flow5], [> | `Figure]) star

  val hr : ([< | hr_attrib], [> | hr]) nullary

  (** {2 Ruby} *)

  (**********************************)
  (*            In Ruby             *)
  (**********************************)
  (* One or more groups of:         *)
  (*phrasing content followed either*)
  (*    by a single rt element,     *)
  (*    or an rp element            *)
  (*       an rt element, and       *)
  (*       another rp element.      *)
  (**********************************)
  val rt :
    ?rp: (rp * rp) ->
    ?a: (([< | common] attrib) list) -> ([< | phrasing] elt) list -> rt

  val rp :
    ?a: (([< | common] attrib) list) -> ([< | phrasing] elt) list -> rp

  val ruby :
    ?a: (([< | common] attrib) list) ->
    ruby_content -> ruby_content list -> [> | `Ruby] elt

  (** {2 Semantic} *)

  val b : ([< | b_attrib], [< | b_content_fun], [> | b]) star

  val i : ([< | i_attrib], [< | i_content_fun], [> | i]) star

  val u : ([< | u_attrib], [< | u_content_fun], [> | u]) star

  val small :
    ([< | small_attrib], [< | small_content_fun], [> | small]) star

  val sub : ([< | sub_attrib], [< | sub_content_fun], [> | sub]) star

  val sup : ([< | sup_attrib], [< | sup_content_fun], [> | sup]) star

  val mark : ([< | mark_attrib], [< | mark_content_fun], [> | mark]) star

  val wbr : ([< | wbr_attrib], [> | wbr]) nullary

  val bdo :
    dir: [< | `Ltr | `Rtl] ->
    ([< | common], [< | phrasing], [> | `Bdo]) star

  val abbr : ([< | abbr_attrib], [< | abbr_content_fun], [> | abbr]) star

  val br : ([< | br_attrib], [> | br]) nullary

  val cite : ([< | cite_attrib], [< | cite_content_fun], [> | cite]) star

  val code : ([< | code_attrib], [< | code_content_fun], [> | code]) star

  val dfn : ([< | dfn_attrib], [< | dfn_content_fun], [> | dfn]) star

  val em : ([< | em_attrib], [< | em_content_fun], [> | em]) star

  val kbd : ([< | kbd_attrib], [< | kbd_content_fun], [> | kbd]) star

  val q : ([< | q_attrib], [< | q_content_fun], [> | q]) star

  val samp : ([< | samp_attrib], [< | samp_content_fun], [> | samp]) star

  val span : ([< | span_attrib], [< | span_content_fun], [> | span]) star

  val strong :
    ([< | strong_attrib], [< | strong_content_fun], [> | strong]) star

  val time : ([< | time_attrib], [< | time_content_fun], [> | time]) star

  val var : ([< | var_attrib], [< | var_content_fun], [> | var]) star

  (** {2 Hypertext} *)

  (********************************)
  (*             In A             *)
  (********************************)
  (*    The target, rel, media,   *)
  (* hreflang, and type attributes*)
  (*  must be omitted if the href *)
  (*   attribute is not present.  *)
  (********************************)
  (*Only phasing instead of flow ?*)
  (********************************)
  (* a's children are transparents*)
  (********************************)
  val a : ([< | a_attrib], 'a, [> | `A of 'a]) star

  (** {2 Edit} *)

  (**********************************)
  (* del's children are transparents*)
  (**********************************)
  val del : ([< | del_attrib], 'a, [> | `Del of 'a]) star

  (**********************************)
  (* ins's children are transparents*)
  (**********************************)
  val ins : ([< | ins_attrib], 'a, [> | `Ins of 'a]) star

  (** {2 Embedded} *)

  val img :
    src: Xml.uri ->
    alt: text ->
    ([< | common | `Height | `Ismap | `Width], [> `Img ])
      nullary

  val iframe : (*| `Srcdoc*)
    ([< | common | `Src | `Name | `Sandbox | `Seamless | `Width | `Height],
     [< | `PCDATA], [> | `Iframe]) star

  val object_ :
    ?params: (([< | `Param] elt) list) ->
    ([<
     | common
     | `Data
     | `Form
     | `Mime_type
     | `Height
     | `Width
     | `Name
     | `Usemap
     ], 'a, [> | `Object of 'a ]) star

  val param : ([< | param_attrib], [> | param]) nullary

  (**********************************)
  (*            In Embed            *)
  (**********************************)
  (*  Any namespace-less attribute  *)
  (* other than name, align, hspace,*)
  (* and vspace  may be specified on*)
  (* the embed element, so long as  *)
  (* its name is XML-compatible and *)
  (* contains no characters in the  *)
  (* range U+0041 to U+005A         *)
  (*(LATIN CAPITAL LETTER A to LATIN*)
  (*CAPITAL LETTER Z).              *)
  (*These attributes are then passed*)
  (*  as parameters to the plugin.  *)
  (**********************************)
  val embed :
    ([< | common | `Src | `Height | `Mime_type | `Width], [> | `Embed])
      nullary

  (**************************************)
  (*         In Audio and Video         *)
  (**************************************)
  (* If the element has a src attribute:*)
  (*   transparent, but with no media   *)
  (*   element descendants.             *)
  (* If the element does not have a src *)
  (* attribute:                         *)
  (*   one or more source elements, then*)
  (*   transparent, but with no media   *)
  (*   element descendants.             *)
  (**************************************)
  val audio :
    ?src:Xml.uri ->
    ?srcs:(([< | `Source] elt) list) ->
    ([< | common | `Preload | `Autoplay | `Loop | `Controls], 'a,
     [> | `Audio of 'a ]) star

  val video :
    ?src:Xml.uri ->
    ?srcs: (([< | `Source] elt) list) ->
    ([<
     | common
     | `Poster
     | `Preload
     | `Autoplay
     | `Loop
     | `Controls
     | `Width
     | `Height
     ], 'a, [> | `Video of 'a]) star

  val canvas : ([< | canvas_attrib], 'a, [> | `Canvas of 'a]) star

  val source : ([< | source_attrib], [> | source]) nullary

  (********************************)
  (*           In Area            *)
  (********************************)
  (* The alt, target, rel, media, *)
  (* hreflang, and type attributes*)
  (*  must be omitted if the href *)
  (*   attribute is not present.  *)
  (********************************)
  val area :
    alt: text ->
    ([<
     | common
     | `Alt
     | `Coords
     | `Shape
     | `Target
     | `Rel
     | `Media
     | `Hreflang
     | `Mime_type
     ], [> | `Area]) nullary

  (* XXX: SC : the current system doesn't allow
     to put <area> tag inside a map (a priori) *)
  val map : ([< | map_attrib], 'a, [> | `A of 'a]) plus

  (** {2 Tables Data} *)

  val caption :
    ([< | caption_attrib], [< | caption_content_fun], [> | caption]) star

  (********************************)
  (*      In Table and Tablex     *)
  (********************************)
  (*    In this order:            *)
  (* optionally a caption element,*)
  (* followed by either           *)
  (*zero or more colgroup elements*)
  (* followed optionally by a     *)
  (*thead element,                *)
  (* followed optionally by a     *)
  (*tfoot element,                *)
  (* followed by either           *)
  (*zero or more tbody elements   *)
  (*or one or more tr elements,   *)
  (* followed optionally by       *)
  (*a tfoot element               *)
  (********************************)
  (*   BUT ONLY ONE FOOT ELEMENT  *)
  (*         CHILD IN TOTAL       *)
  (********************************)
  val table :
    ?caption: ([< | `Caption] elt) ->
    ?columns: (([< | `Colgroup] elt) list) ->
    ?thead: ([< | `Thead] elt) ->
    ?tfoot: ([< | `Tfoot] elt) ->
    ([< | common | `Summary], [< | `Tr], [> | `Table]) plus

  val tablex :
    ?caption: ([< | `Caption] elt) ->
    ?columns: (([< | `Colgroup] elt) list) ->
    ?thead: ([< | `Thead] elt) ->
    ?tfoot: ([< | `Tfoot] elt) ->
    ([< | common | `Summary], [< | `Tbody], [> | `Table]) star

  (********************************)
  (*          In Colgroup         *)
  (********************************)
  (*   If span attribute is:      *)
  (*       -present: Empty.       *)
  (*       -absent: Zero or more  *)
  (*                col elements. *)
  (********************************)
  val colgroup :
    ([< | colgroup_attrib], [< | colgroup_content_fun], [> | colgroup])
      star

  val col : ([< | col_attrib], [> | col]) nullary

  val thead :
    ([< | thead_attrib], [< | thead_content_fun], [> | thead]) star

  val tbody :
    ([< | tbody_attrib], [< | tbody_content_fun], [> | tbody]) star

  val tfoot :
    ([< | tfoot_attrib], [< | tfoot_content_fun], [> | tfoot]) star

  val td : ([< | td_attrib], [< | td_content_fun], [> | td]) star

  val th : ([< | th_attrib], [< | th_content_fun], [> | th]) star

  (****************************************)
  (*                 In Tr                *)
  (****************************************)
  (*If the parent node is a thead element:*)
  (*      Zero or more th elements        *)
  (* Otherwise:                           *)
  (*    Zero or more td or th elements    *)
  (****************************************)
  val tr : ([< | tr_attrib], [< | tr_content_fun], [> | tr]) star

  (** {2 Forms} *)

  val form : ([< | form_attrib], [< | form_content_fun], [> | form]) plus

  val fieldset :
    ?legend: ([ | `Legend ] elt) ->
    ([< | common | `Disabled | `Form | `Name], [< | flow5],
     [> | `Fieldset]) star

  val legend :
    ([< | legend_attrib], [< | legend_content_fun], [> | legend]) star

  (** Label authorizes only one  control inside them
      that should be labelled with a [for] attribute
      (although it is not necessary). Such constraints are not currently
      enforced by the type-system *)
  val label :
    ([< | label_attrib], [< | label_content_fun], [> | label]) star

  (** If the [type] attribute is not "hidden", must be considered
      as interactive. Distinction not made for now. *)
  val input : ([< | input_attrib], [> | input]) nullary

  (********************************)
  (*          In Button           *)
  (********************************)
  (* The formaction, formenctype, *)
  (*  formmethod, formnovalidate, *)
  (*  and formtarget must not be  *)
  (*  specified if the element's  *)
  (* type  attribute is not in the*)
  (*     Submit Button  state.    *)
  (********************************)
  val button :
    ([< | button_attrib], [< | button_content_fun], [> | button]) star

  val select :
    ([< | select_attrib], [< | select_content_fun], [> | select]) star

  val datalist :
    ?children:
    [<
    | `Options of ([< | `Option] elt) list
    | `Phras of ([< | phrasing] elt) list
    ] -> ([< | common], [> | `Datalist]) nullary

  val optgroup :
    label: text ->
    ([< | common | `Disabled | `Label], [< | `Option], [> | `Optgroup])
      star

  val option :
    ([< | option_attrib], [< | option_content_fun], [> | selectoption])
      unary

  val textarea :
    ([< | textarea_attrib], [< | textarea_content_fun], [> | textarea])
      unary

  val keygen : ([< | keygen_attrib], [> | keygen]) nullary

  val progress :
    ([< | progress_attrib], [< | progress_content_fun], [> | progress])
      star

  val meter :
    ([< | meter_attrib], [< | meter_content_fun], [> | meter]) star

  val output_elt :
    ([< | output_elt_attrib], [< | output_elt_content_fun], [> | output_elt]) star

  (** {2 Data} *)

  val pcdata : string -> [> | `PCDATA] elt

  val entity : string -> [> | `PCDATA] elt

  val space : unit -> [> | `PCDATA] elt

  val cdata : string -> [> | `PCDATA] elt

  (* GK *)
  val cdata_script : string -> [> | `PCDATA] elt

  (* GK *)
  val cdata_style : string -> [> | `PCDATA] elt


  (** {2 Interactive} *)

  val details :
    [< | `Summary] elt ->
    ([< | common | `Open], [< | flow5] elt, [> | `Details]) star

  val summary :
    ([< | summary_attrib], [< | summary_content_fun], [> | summary]) star

  val command :
    label: text ->
    ([<
     | common
     | `Icon
     | `Disabled
     | `Checked
     | `Radiogroup
     | `Command_Type
     ], [> | `Command]) nullary

  val menu :
    ?child:
    [<
    | `Lis of ([< | `Li of [< | common]] elt) list
    | `Flows of ([< | flow5] elt) list
    ] -> ([< | common | `Label | `Menu_Type], [> | `Menu]) nullary

  (** {2 Scripting} *)

  val script :
    ([< | script_attrib], [< | script_content_fun], [> | script]) unary

  (****************************************************)
  (*                   In Noscript                    *)
  (****************************************************)
  (*When scripting is DISABLED, IN a HEAD element:    *)
  (*   in any order, zero or more link elements,      *)
  (*   zero or more style elements, and zero or more  *)
  (*   meta elements.                                 *)
  (*When scripting is DISABLED, NOT IN a HEAD element:*)
  (*   transparent, but there must be no noscript     *)
  (*   element descendants.                           *)
  (*When scripting is ENABLED, IN a HEAD element:     *)
  (*   only text, except that invoking the HTML       *)
  (*   fragment parsing algorithm with the noscript   *)
  (*   element as the context element and the text    *)
  (*   contents as the input must result in a list of *)
  (*   nodes that consists only of link, style, and   *)
  (*   meta elements that would be conforming if they *)
  (*   were children of the noscript element, and no  *)
  (*   parse errors.                                  *)
  (*When scripting is ENABLED, NOT IN a HEAD element: *)
  (*   only text, except that the text must be such   *)
  (*   that running the following algorithm results in*)
  (*   a conforming document with no noscript elements*)
  (*   and no script elements, and such that no step  *)
  (*   in the algorithm causes an HTML parser to flag *)
  (*   a parse error                                  *)
  (****************************************************)
  (* PLUS ?? *)
  val noscript :
    ([< | noscript_attrib], [< | noscript_content_fun], [> | noscript])
      plus

  val meta : ([< | meta_attrib], [> | meta]) nullary

  (** {2 Style Sheets} *)

  (*********************************)
  (*            In Style           *)
  (*********************************)
  (* the content model depends on  *)
  (*the value of the type attribute*)
  (*********************************)
  (*          BUT WHAT ???         *)
  (* SC: contenttype defaults to   *)
  (*  text/css                     *)
  (*********************************)
  val style :
    ([< | style_attrib], [< | style_content_fun], [> | style]) star

  (** {2 Link} *)

  val link :
    rel: linktypes ->
    href: Xml.uri ->
    ([<
     | common
     | `Hreflang
     | `Media
     | `Rel
     | `Href
     | `Sizes
     | `Mime_type
     ], [> | `Link]) nullary

  (** {1 Tools} *)

  val tot : Xml.elt -> 'a elt
  val totl : Xml.elt list -> ('a elt) list
  val toelt : 'a elt -> Xml.elt
  val toeltl : ('a elt) list -> Xml.elt list

  (** *)

  type doc = [ `Html ] elt
  val doc_toelt : doc -> Xml.elt


  module Unsafe : sig
    (** Unsafe features. Warning using this module can break
        HTML5 validity and may introduce security problems like
        code injection.
        Use it with care.
    *)

    (** Insert raw text without any encoding *)
    val data : string -> 'a elt

    (** Insert an XML node that is not implemented in this module.
        If it is a standard HTML5 node which is missing,
        please report to the Ocsigen team.
    *)
    val node : string -> ?a:'a attrib list -> 'b elt list -> 'c elt

    (** Insert an XML node without children
        that is not implemented in this module.
        If it is a standard HTML5 node which is missing,
        please report to the Ocsigen team.
    *)
    val leaf : string -> ?a:'a attrib list -> unit -> 'b elt

    (** Remove phantom type annotation on an element,
        to make it usable everywhere.
    *)
    val coerce_elt : 'a elt -> 'b elt

    (** Insert an attribute that is not implemented in this module.
        If it is a standard HTML5 attribute which is missing,
        please report to the Ocsigen team.
    *)
    val string_attrib : string -> string -> 'a attrib

    (** Same, for float attribute *)
    val float_attrib : string -> float -> 'a attrib

    (** Same, for int attribute *)
    val int_attrib : string -> int -> 'a attrib

    (** Same, for URI attribute *)
    val uri_attrib : string -> uri -> 'a attrib

    (** Same, for a space separated list of values *)
    val space_sep_attrib : string -> string list -> 'a attrib

    (** Same, for a comma separated list of values *)
    val comma_sep_attrib : string -> string list -> 'a attrib

  end

end
