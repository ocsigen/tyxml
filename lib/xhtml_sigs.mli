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

open Xhtml_types

module type T_01_01 = sig

  module Xml : Xml_sigs.T
  module Info : Xml_sigs.Info

  (** The elements, attributes, attribute types and data types are given names
      that match the names in the W3C recommendation as closely as allowed by
      a strict typing discipline and the lexical conventions of O'Caml:
      {ul
      {- {e elements} are implemented as O'Caml constructors with the same name as
      in the W3C recommendation.  The domain and codomain are specified as ['a elt],
      where ['a] is a concrete phantom type build out of polymorphic variants.}
      {- {e attributes} are implemented as O'Caml constructors with [a_] prefixed to the
      name.  The name is the same as in the W3C recommendation, unless an additional
      prefix is required to disambiguate:
      {ul
      {- [a_fs_rows] and [a_fs_cols] instead of [a_rows] and [a_cols] for framesets,
      because of the different argument types.}}}
      {- {e attribute types} are implemented as O'Caml types that all have the same names
      as in the W3C recommendation, but are all lowercase.}
      {- {e data types} are also implemented as O'Caml types that all have the same names
      as in the W3C recommendation and are again all lowercase.}}

      Finite sets of alternatives are mapped to polymorphic variants.

      The phantom type is always the {e most general} required by any (supported)
      version of the standard.  Type discipline is enforced by exporting or not-exporting
      the corresponding constructor.

      The type defining group of html elements are in {!Xhtml_types} *)

  type uri = Xml.uri
  val string_of_uri : uri -> string
  val uri_of_string : string -> uri

  (** {1 Common Attributes} *)

  type +'a attrib
  val to_xmlattribs : 'a attrib list -> Xml.attrib list (* VB *)
  val to_attrib : Xml.attrib -> 'a attrib (* GH *)

  (** ['a] is known as a {i phantom type}.  The implementation is
      actually monomorphic (the different element types are
      distinguished by a homogeneous variable, such as their textual
      representation) and the type variable [`a] is just used by the
      type checker.

      NB: It might be possible to use polymorphic variants directly,
      without phantom types, but the implementation is likely to
      be more involved. *)

  (** {2 Core} *)

  (** This attribute assigns a class name or set of class names to an
      element. Any number of elements may be assigned the same class
      name or names.  *)
  val a_class : nmtokens -> [>`Class] attrib

  (** This attribute assigns a name to an element. This name must be
      unique in a document. *)
  val a_id : id -> [>`Id] attrib

  (** This attribute offers advisory information about the element for
      which it is set. *)
  val a_title : cdata -> [>`Title] attrib

  (** Values of the title attribute may be rendered by user agents in a
      variety of ways. For instance, visual browsers frequently display
      the title as a {i tool tip} (a short message that appears when the
      pointing device pauses over an object). Audio user agents may
      speak the title information in a similar context.  *)

  (** The title attribute has an additional role when used with the [link]
      element to designate an external style sheet. Please consult the
      section on links and style sheets for details.  *)

  (** {2 I18N} *)

  val a_xml_lang : nmtoken -> [>`XML_lang] attrib


  (** {2 Style}
      The Style collection is deprecated, because the Style Attribute Module is
      deprecated. *)

  (** {2 Events} *)

  (** Javascript events *)

  val a_onblur : Xml.event_handler -> [>`OnBlur] attrib
  val a_onclick : Xml.event_handler -> [>`OnClick] attrib
  val a_ondblclick : Xml.event_handler -> [>`OnDblClick] attrib
  val a_onchange : Xml.event_handler -> [>`OnChange] attrib
  val a_onfocus : Xml.event_handler -> [>`OnFocus] attrib
  val a_onload : Xml.event_handler -> [>`OnLoad] attrib
  val a_onunload : Xml.event_handler -> [>`OnUnload] attrib
  val a_onreset : Xml.event_handler -> [>`OnReset] attrib
  val a_onselect : Xml.event_handler -> [>`OnSelect] attrib
  val a_onsubmit : Xml.event_handler -> [>`OnSubmit] attrib
  val a_onmousedown : Xml.event_handler -> [>`OnMouseDown] attrib
  val a_onmouseup : Xml.event_handler -> [>`OnMouseUp] attrib
  val a_onmouseover : Xml.event_handler -> [>`OnMouseOver] attrib
  val a_onmousemove : Xml.event_handler -> [>`OnMouseMove] attrib
  val a_onmouseout : Xml.event_handler -> [>`OnMouseOut] attrib
  val a_onkeypress : Xml.event_handler -> [>`OnKeyPress] attrib
  val a_onkeydown : Xml.event_handler -> [>`OnKeyDown] attrib
  val a_onkeyup : Xml.event_handler -> [>`OnKeyUp] attrib


  (** {1 Modules, Element Sets and Attributes } *)


  val a_profile : Xml.uri -> [>`Profile] attrib
  val a_version : cdata -> [>`Version] attrib
  val a_xmlns : [< `W3_org_1999_xhtml ] -> [>`XMLns] attrib
  val a_cite : Xml.uri -> [>`Cite] attrib
  val a_xml_space : [< `Preserve ] -> [>`XML_space] attrib

  (** This attribute assigns an access key to an element. An
      access key is a single character from the document character
      set. NB: authors should consider the input method of the
      expected reader when specifying an accesskey. *)
  val a_accesskey : character -> [>`Accesskey] attrib

  (** This attribute specifies the character encoding of the
      resource designated by the link. Please consult the section
      on character encodings for more details. *)
  val a_charset : charset -> [>`Charset] attrib

  val a_accept_charset : charset -> [>`Accept_charset] attrib
  val a_accept : contenttype -> [>`Accept] attrib

  (** This attribute specifies the location of a Web resource,
      thus defining a link between the current element (the source
      anchor) and the destination anchor defined by this
      attribute. *)
  val a_href : Xml.uri -> [>`Href] attrib

  (** This attribute specifies the base language of the resource
      designated by href and may only be used when href is
      specified. *)
  val a_hreflang : languagecode -> [>`Hreflang] attrib

  (** This attribute describes the relationship from the current
      document to the anchor specified by the href attribute. The
      value of this attribute is a space-separated list of link
      types. *)
  val a_rel : linktypes -> [>`Rel] attrib

  (** This attribute is used to describe a reverse link from the
      anchor specified by the href attribute to the current
      document. The value of this attribute is a space-separated
      list of link types. *)
  val a_rev : linktypes -> [>`Rev] attrib

  (** This attribute specifies the position of the current element
      in the tabbing order for the current document. This value
      must be a number between 0 and 32767. User agents should
      ignore leading zeros. *)
  val a_tabindex : number -> [>`Tabindex] attrib

  (** This attribute gives an advisory hint as to the content type
      of the content available at the link target address. It
      allows user agents to opt to use a fallback mechanism rather
      than fetch the content if they are advised that they will
      get content in a content type they do not support.Authors
      who use this attribute take responsibility to manage the
      risk that it may become inconsistent with the content
      available at the link target address. *)
  val a_type : contenttype -> [>`Type] attrib

  val a_datetime : cdata -> [>`Datetime] attrib


  (** {3 Bi-directional Text Attributes} *)

  val a_dir : [< `Ltr | `Rtl ] -> [>`Dir] attrib

  (** {3 Forms attributes} *)

  (** This attribute specifies a form processing agent. User agent
      behavior for a value other than an HTTP URI is undefined. *)
  val a_action : Xml.uri -> [>`Action] attrib

  (** When the [type] attribute has the value ["radio"] or
      ["checkbox"], this boolean attribute specifies that the
      button is on. User agents must ignore this attribute for
      other control types. *)
  val a_checked : [< `Checked ] -> [>`Checked] attrib

  (** This attribute specifies the visible width in average
      character widths. Users should be able to enter longer lines
      than this, so user agents should provide some means to
      scroll through the contents of the control when the contents
      extend beyond the visible area. User agents may wrap visible
      text lines to keep long lines visible without the need for
      scrolling. *)
  val a_cols : number -> [>`Cols] attrib

  val a_enctype : contenttype -> [>`Enctype] attrib
  val a_for : idref -> [>`For] attrib
  val a_maxlength : number -> [>`Maxlength] attrib
  val a_method : [< `Get | `Post ] -> [>`Method] attrib
  val a_multiple : [< `Multiple ] -> [>`Multiple] attrib

  (** This attribute assigns the control name. *)
  val a_name : cdata -> [>`Name] attrib

  (** This attribute specifies the number of visible text
      lines. Users should be able to enter more lines than this,
      so user agents should provide some means to scroll through
      the contents of the control when the contents extend beyond
      the visible area. *)
  val a_rows : number -> [>`Rows] attrib

  (** When set, this boolean attribute specifies that this option
      is pre-selected. *)
  val a_selected : [< `Selected ] -> [>`Selected] attrib

  val a_size : number -> [>`Size] attrib
  val a_src : Xml.uri -> [>`Src] attrib
  val a_input_type :
    [< `Text | `Password | `Checkbox | `Radio | `Submit | `Reset | `Hidden | `File | `Image | `Button ] ->
    [>`Input_Type] attrib

  (** This attribute specifies the initial value of the
      control. If this attribute is not set, the initial value is
      set to the contents of the [option] element. *)
  val a_value : cdata -> [>`Value] attrib

  val a_value_type : [< `Data | `Ref | `Object ] -> [>`Value_Type] attrib

  val a_disabled : [< `Disabled ] -> [>`Disabled] attrib
  val a_readonly : [< `ReadOnly ] -> [>`ReadOnly] attrib
  val a_button_type : [< `Button | `Submit | `Reset ] -> [>`Button_Type] attrib

  val a_label : text -> [> `Label ] attrib


  (** {2 Attributes for tables} *)

  val a_abbr : text -> [>`Abbr] attrib
  val a_align : [< `Left | `Center | `Right | `Justify | `Char ] -> [>`Align] attrib
  val a_axis : cdata -> [>`Axis] attrib
  val a_colspan : number -> [>`Colspan] attrib
  val a_headers : idrefs -> [>`Headers] attrib
  val a_rowspan : number -> [>`Rowspan] attrib
  val a_scope : [< `Row | `Col | `Rowgroup | `Colgroup ] -> [>`Scope] attrib
  val a_summary : text -> [>`Summary] attrib
  val a_valign : [< `Top | `Middle | `Bottom | `Baseline ] -> [>`Valign] attrib

  val a_border : pixels -> [>`Border] attrib
  val a_cellpadding : length -> [>`Cellpadding] attrib
  val a_cellspacing : length -> [>`Cellspacing] attrib
  val a_datapagesize : cdata -> [>`Datapagesize] attrib
  val a_frame :
    [< `Void | `Above | `Below | `Hsides | `LHS | `RHS
    | `Vsides | `Box | `Border ] -> [>`Frame] attrib
  val a_rules : [< `None | `Groups | `Rows | `Cols | `All ] -> [>`Rules] attrib
  val a_char : character -> [>`Char] attrib
  val a_charoff : length -> [>`Charoff] attrib
  val a_span : number -> [>`Span] attrib

  val a_alt : text -> [>`Alt] attrib
  val a_height : length -> [>`Height] attrib
  val a_longdesc : Xml.uri -> [>`Longdesc] attrib
  val a_width : length -> [>`Width] attrib

  (** {2 Attributes for client-side image map} *)

  type shape = [ `Rect | `Circle | `Poly | `Default ]
  val a_shape : shape -> [>`Shape] attrib
  val a_coords : int list -> [>`Coords] attrib
  val a_nohref : [< `Nohref ] -> [>`Nohref] attrib
  val a_usemap : idref -> [>`Usemap] attrib

  (** {2 Attributes for Server-side Image Map} *)

  val a_ismap : [< `Ismap ] -> [>`Ismap] attrib

  (** {2 Attributes for Object} *)

  val a_declare : [< `Declare ] -> [> `Declare ] attrib
  val a_classid : Xml.uri -> [> `Classid ] attrib
  val a_codebase : Xml.uri -> [> `Codebase ] attrib
  val a_data : Xml.uri -> [> `Data ] attrib
  val a_codetype : contenttype -> [>`Codetype] attrib
  val a_archive : Xml.uri list -> [>`Archive] attrib
  val a_standby : text -> [>`Standby] attrib

  (** {2 Attributes for Frames } *)

  val a_fs_rows : multilengths -> [>`FS_Rows] attrib
  val a_fs_cols : multilengths -> [>`FS_Cols] attrib
  val a_frameborder : [< `Zero | `One ] -> [>`Frameborder] attrib
  val a_marginheight : pixels -> [>`Marginheight] attrib
  val a_marginwidth : pixels -> [>`Marginwidth] attrib
  val a_noresize : [< `Noresize ] -> [>`Noresize] attrib
  val a_scrolling : [< `Yes | `No | `Auto ] -> [>`Scrolling] attrib

  val a_target : frametarget -> [>`Target] attrib


  (** {2 Attributes for metadata} *)

  val a_content : cdata -> [>`Content] attrib
  val a_http_equiv : nmtoken -> [>`Http_equiv] attrib
  val a_scheme : cdata -> [>`Scheme] attrib


  val a_defer : [< `Defer ] -> [>`Defer] attrib

  (** {3 Style attributes }*)

  val a_media : mediadesc -> [>`Media] attrib
  val a_style : string -> [>`Style_Attr] attrib


  (** {1 Elements} *)

  type +'a elt

  (** {2 Element Constructor Types} *)

  type ('a, 'b) nullary = ?a:('a attrib list) -> unit -> 'b elt
  type ('a, 'b, 'c) unary = ?a:('a attrib list) -> 'b elt -> 'c elt
  type ('a, 'b, 'c, 'd) binary = ?a:('a attrib list) -> 'b elt -> 'c elt -> 'd elt
  type ('a, 'b, 'c, 'd, 'e, 'f) quadry= ?a:('a attrib list) -> 'b elt -> 'c elt -> 'd elt -> 'e elt -> 'f elt

  (** Star '*' denotes any number of children, uncluding zero. *)
  type ('a, 'b, 'c) star = ?a:('a attrib list) -> 'b elt list -> 'c elt

  (** Plus '+' requires at least one child.  *)
  type ('a, 'b, 'c) plus = ?a:('a attrib list) -> 'b elt -> 'b elt list -> 'c elt

  (** {2 Structure} *)

  type html = [`Html] elt

  val html : ?a:([< i18n | `Version | `XMLns | `Id ] attrib list) -> [< `Head ] elt -> [< `Body | `Frameset ] elt -> html
  val head : ?a:([< i18n | `Profile | `Id ] attrib list) -> [< `Base | `Title ] elt ->
    [< `Meta | `Link | `Style | `Object | `Script ] elt list -> [>`Head] elt
  val title : ([< title_attrib] , [< title_content], [> title ]) unary
  val body : ([< body_attrib] , [< body_content], [> body ]) star

  (** {2 Data} *)

  val pcdata : string -> [>`PCDATA] elt
  val entity : string -> [>`PCDATA] elt
  val space : unit -> [>`PCDATA] elt
  val cdata : string -> [>`PCDATA] elt (* GK *)
  val cdata_script : string -> [>`PCDATA] elt (* GK *)
  val cdata_style : string -> [>`PCDATA] elt (* GK *)
  (**/**)
  val unsafe_data : string -> 'a elt
  (**/**)

  (** {2 Text} *)

  val h1 : ([< h1_attrib] , [< h1_content], [> h1 ]) star
  val h2 : ([< h2_attrib] , [< h2_content], [> h2 ]) star
  val h3 : ([< h3_attrib] , [< h3_content], [> h3 ]) star
  val h4 : ([< h4_attrib] , [< h4_content], [> h4 ]) star
  val h5 : ([< h5_attrib] , [< h5_content], [> h5 ]) star
  val h6 : ([< h6_attrib] , [< h6_content], [> h6 ]) star

  val address : ([< address_attrib] , [< address_content], [> address ]) star
  val blockquote : ([< common | `Cite ],
		    [< `PCDATA | block ], [>`Blockquote]) star
  val div : ([< div_attrib] , [< div_content], [> div ]) star
  val p : ([< p_attrib] , [< p_content], [> p ]) star
  val pre : ([< common | `XML_space ], [< `PCDATA | precontent ], [>`Pre]) star

  val abbr : ([< abbr_attrib] , [< abbr_content], [> abbr ]) star
  val acronym : ([< acronym_attrib] , [< acronym_content], [> acronym ]) star
  val br : ([< br_attrib ], [> br]) nullary
  val cite : ([< cite_attrib] , [< cite_content], [> cite ]) star
  val code : ([< code_attrib] , [< code_content], [> code ]) star
  val dfn : ([< dfn_attrib] , [< dfn_content], [> dfn ]) star
  val em : ([< em_attrib] , [< em_content], [> em ]) star
  val kbd : ([< kbd_attrib] , [< kbd_content], [> kbd ]) star
  val q : ([< q_attrib] , [< q_content], [> q ]) star
  val samp : ([< samp_attrib] , [< samp_content], [> samp ]) star
  val span : ([< span_attrib] , [< span_content], [> span ]) star
  val strong : ([< strong_attrib] , [< strong_content], [> strong ]) star
  val var : ([< var_attrib] , [< var_content], [> var ]) star

  (** {2 Hypertext} *)

  val a : ([< a_attrib] , [< a_content], [> a ]) star

  (** {2 List} *)

  val dl : ([< dl_attrib] , [< dl_content], [> dl ]) plus
  val ol : ([< ol_attrib] , [< ol_content], [> ol ]) plus
  val ul : ([< ul_attrib] , [< ul_content], [> ul ]) plus
  val dd : ([< dd_attrib] , [< dd_content], [> dd ]) star
  val dt : ([< dt_attrib] , [< dt_content], [> dt ]) star
  val li : ([< li_attrib] , [< li_content], [> li ]) star

  (** {2 Presentation} *)

  val hr : ([< hr_attrib ], [> hr]) nullary
  val b : ([< b_attrib] , [< b_content], [> b ]) star
  val big : ([< big_attrib] , [< big_content], [> big ]) star
  val i : ([< i_attrib] , [< i_content], [> i ]) star
  val small : ([< small_attrib] , [< small_content], [> small ]) star
  val sub : ([< sub_attrib] , [< sub_content], [> sub ]) star
  val sup : ([< sup_attrib] , [< sup_content], [> sup ]) star
  val tt : ([< tt_attrib] , [< tt_content], [> tt ]) star

  (* CH *)
  val bdo : dir:[< `Ltr | `Rtl ]   ->([< bdo_attrib] , [< bdo_content], [> bdo ]) star
  (* CH *)

  val area : alt:text  ->([< area_attrib ], [> area]) nullary

  val map : id:id  ->([< map_attrib] , [< map_content], [> map ]) plus

  val del : ([< del_attrib] , [< del_content], [> del ]) star
  val ins : ([< ins_attrib] , [< ins_content], [> ins ]) star
  val script : contenttype:contenttype  ->([< script_attrib] , [< script_content], [> script ]) unary
  val noscript : ([< noscript_attrib] , [< noscript_content], [> noscript ]) plus

  (** {2 Forms} *)

  (** {3 Basic Forms} *)

  (** One can use [open Basic_Forms] to enable basic forms. *)

  module Basic_Forms : sig

    val form : action:Xml.uri  ->([< form_attrib] , [< form_content], [> form ]) plus
    val input : ([< input_attrib ], [> input]) nullary
    val label : ([< label_attrib] , [< label_content], [> label ]) star
    val option : ([< option_attrib] , [< option_content], [> selectoption ]) unary
    val select : ([< select_attrib] , [< select_content], [> select ]) plus
    val textarea : rows:number  -> cols:number  ->([< textarea_attrib] , [< textarea_content], [> textarea ]) unary
  end

  (** {3 Forms} *)

  (** Generic forms. WARNING: If you find a bug or if something is missing please send a bug report to the Ocsigen project! -- VB *)
  val form : action:Xml.uri  ->([< form_attrib] , [< form_content], [> form ]) plus
  val input : ([< input_attrib ], [> input]) nullary
  val label : ([< label_attrib] , [< label_content], [> label ]) star
  val optgroup : label:text  ->([< optgroup_attrib] , [< optgroup_content], [> optgroup ]) plus
  val option : ([< option_attrib] , [< option_content], [> selectoption ]) unary
  val select : ([< select_attrib] , [< select_content], [> select ]) plus
  val textarea : rows:number  -> cols:number  ->([< textarea_attrib] , [< textarea_content], [> textarea ]) unary
  val fieldset : ([< fieldset_attrib] , [< fieldset_content], [> fieldset ]) star
  val legend : ([< legend_attrib] , [< legend_content], [> legend ]) star
  val button : ([< button_attrib] , [< button_content], [> button ]) star

  (** {2 Tables} *)

  (** {3 Basic Tables} *)

  (** One can use [open Basic_Tables] to switch globally to basic tables. *)

  module Basic_Tables : sig

    val a_align : [< `Left | `Center | `Right ] -> [>`Align] attrib
    val a_scope : [< `Row | `Col ] -> [>`Scope] attrib
    val a_valign : [< `Top | `Middle | `Bottom ] -> [>`Valign] attrib

    val caption : ([< caption_attrib] , [< caption_content], [> caption ]) star
    val table : ?caption:([< `Caption ] elt)  ->([< table_attrib] , [< table_content], [> table ]) plus
    val td : ([< td_attrib] , [< td_content], [> td ]) star
    val th : ([< th_attrib] , [< th_content], [> th ]) star
    val tr : ([< tr_attrib] , [< tr_content], [> tr ]) plus
  end

  (** {3 Tables} *)

  val caption : ([< caption_attrib] , [< caption_content], [> caption ]) star

  val table : ?caption:([< `Caption ] elt) ->      ?columns:([< `Cols of ([< `Col ] elt list)
							     | `Colgroups of ([< `Colgroup ] elt list) ]) ->        ([< common | `Border | `Cellpadding | `Cellspacing | `Datapagesize         | `Frame | `Rules | `Summary | `Width ], [< `Tr ], [>`Table]) plus

  val tablex : ?caption:([< `Caption ] elt) ->      ?columns:([< `Cols of ([< `Col ] elt list)
							      | `Colgroups of ([< `Colgroup ] elt list) ]) ->        ?thead:([< `Thead ] elt) -> ?tfoot:([< `Tfoot ] elt) ->          ([< common | `Border | `Cellpadding | `Cellspacing | `Datapagesize           | `Frame | `Rules | `Summary | `Width ], [< `Tbody ], [>`Table]) plus

  val td : ([< td_attrib] , [< td_content], [> td ]) star
  val th : ([< th_attrib] , [< th_content], [> th ]) star
  val tr : ([< tr_attrib] , [< tr_content], [> tr ]) plus

  val col : ([< col_attrib ], [> col]) nullary
  val colgroup : ([< colgroup_attrib] , [< colgroup_content], [> colgroup ]) star
  val thead : ([< thead_attrib] , [< thead_content], [> thead ]) plus
  val tbody : ([< tbody_attrib] , [< tbody_content], [> tbody ]) plus
  val tfoot : ([< tfoot_attrib] , [< tfoot_content], [> tfoot ]) plus

  (** {2 Image} *)

  val img : src:Xml.uri  -> alt:text  ->([< img_attrib ], [> img]) nullary

  (** {2 Object} VB *)

  val object_ : ([< object__attrib] , [< object__content], [> object_ ]) star

  val param : name:text  ->([< param_attrib ], [> param]) nullary

  (** {2 Frames} *)

  val frameset : ?noframes:([< `Noframes ] elt)  ->([< frameset_attrib] , [< frameset_content], [> frameset ]) plus

  val frame : src:Xml.uri  ->([< frame_attrib ], [> frame]) nullary

  val noframes : ([< noframes_attrib], [< noframes_content], [> noframes ]) unary

  val iframe : ([< iframe_attrib], [< iframe_content], [> iframe ]) star


  (** {2 Meta} *)

  val meta : content:cdata -> ([< meta_attrib ], [> meta]) nullary

  (** {2 Style Sheets} *)

  val style : contenttype:contenttype -> ([< style_attrib] , [< style_content], [> style ]) star

  (** {2 Link} *)

  val link : ([< link_attrib ], [> link]) nullary

  (** {2 Base} *)

  (* in the DTD of xHTML1.1 xmlns attribute
     in the doc of xHTML1.1 id attribute *)
  val base : href:Xml.uri  ->([< base_attrib ], [> base]) nullary

  (** {2 Ruby} *)

  val ruby_simple1 : ?a:([< common] attrib list) ->      [< `Rb ] elt -> [< `Rt ] elt -> [>`Ruby_simple1] elt
  val ruby_simple2 : ?a:([< common] attrib list) ->      [< `Rb ] elt -> [< `Rp ] elt -> [< `Rt ] elt -> [< `Rp ] elt -> [>`Ruby_simple2] elt
  val ruby_complex : ?a:([< common] attrib list) ->      [< `Rbc ] elt -> [< `Rtc_complex ] elt -> [>`Ruby_complex] elt

  val rbc : ([< rbc_attrib] , [< rbc_content], [> rbc ]) plus
  val rtc : ([< rtc_attrib] , [< rtc_content], [> rtc ]) plus
  val rtc_complex : ([< rtc_complex_attrib] , [< rtc_complex_content], [> rtc_complex ]) plus
  val rb : ([< rb_attrib] , [< rb_content], [> rb ]) star
  val rt : ([< rt_attrib] , [< rt_content], [> rt ]) star
  val rt_complex : ([< rt_complex_attrib] , [< rt_complex_content], [> rt_complex ]) star
  val rp : ([< rp_attrib] , [< rp_content], [> rp ]) star

  val a_rbspan : number -> [>`Rbspan] attrib

  (** {2 ... } *)

  val tot : Xml.elt -> 'a elt
  val totl : Xml.elt list -> 'a elt list
  val toelt : 'a elt -> Xml.elt
  val toeltl : 'a elt list -> Xml.elt list

  module Unsafe : sig
    (** Unsafe features. Warning using this module can break
        validity and may introduce security problems like
        code injection.
        Use it with care.
    *)

    (** Insert raw text without any encoding *)
    val data : string -> 'a elt

    (** Insert an XML node that is not implemented in this module.
        If it is a standard XHTML node which is missing,
        please report to the Ocsigen team.
    *)
    val node : string -> ?a:'a attrib list -> 'b elt list -> 'c elt

    (** Insert an XML node without children
        that is not implemented in this module.
        If it is a standard XHTML node which is missing,
        please report to the Ocsigen team.
    *)
    val leaf : string -> ?a:'a attrib list -> unit -> 'b elt

    (** Insert an attribute that is not implemented in this module.
        If it is a standard XHTML attribute which is missing,
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

  (** {2 ... }*)

  type doc = [ `Html ] elt
  val doc_toelt : doc -> Xml.elt

end

module type T_01_00 = sig

  include T_01_01
  val a_name_01_00 : cdata -> [>`Name_01_00] attrib

end

module type T = T_01_01
