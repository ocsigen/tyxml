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
 * Foundation, Inc., 51 Franklin Street, Suite 500, Boston, MA 02110-1301, USA.
*)

(** SVG signatures for the functorial interface. *)

(** Signature of typesafe constructors for SVG documents. *)
module type T = sig

  (** SVG elements.

      Element constructors are in section {!elements}. Most elements constructors
      are either {{!nullary}nullary}, {{!unary}unary} or {{!star}star},
      depending on the number of children they accept.
      Children are usually given as a list of elements.
      {{!txt}txt} is used for text.

      The type variable ['a] is used to track the element's type. This
      allows the OCaml typechecker to check SVG validity.

      Note that the concrete implementation of this type can vary.
      See {!Xml} for details.
  *)
  type +'a elt

  (** A complete SVG document. *)
  type doc = [ `Svg ] elt

  (** SVG attributes

      Attribute constructors are in section {!attributes} and their name starts
      with [a_]. Attributes are given to elements with the [~a] optional argument.

      Similarly to {{!elt}elt}, attributes use the OCaml type system to enforce
      Html validity.

      In some cases, attributes have to be disambiguated.
      The [max] attribute has two version,
      {!a_fill} and {!a_animation_fill},
      depending on the element.
      Such disambiguated attribute will contain the name of the associated element.
  *)
  type +'a attrib

  (** Underlying XML data-structure

      The type variables in {!elt} and {!attrib} are know as {i phantom types}.
      The implementation, defined here, is actually monomorphic.

      In particular, tyxml doesn't impose any overhead over the underlying
      representation. The {!tot} and {!toelt} functions allows to convert
      between the typed and the untyped representation without any cost.

      Note that some implementation may not be iterable or printable, such as the
      Dom representation exposed by js_of_ocaml.
  *)
  module Xml : Xml_sigs.T

  (** [wrap] is a container for elements and values.

      In most cases, ['a wrap = 'a]. For [R] modules (in eliom or js_of_ocaml),
      It will be {!React.S.t}.
  *)
  type 'a wrap = 'a Xml.W.t

  (** [list_wrap] is a containre for list of elements.

      In most cases, ['a list_wrap = 'a list]. For [R] modules (in eliom or js_of_ocaml),
      It will be {!ReactiveData.RList.t}.
  *)
  type 'a list_wrap = 'a Xml.W.tlist

  (** A nullary element is an element that doesn't have any children. *)
  type ('a, 'b) nullary = ?a: (('a attrib) list) -> unit -> 'b elt

  (** A unary element is an element that have exactly one children. *)
  type ('a, 'b, 'c) unary = ?a: (('a attrib) list) -> 'b elt wrap -> 'c elt

  (** A star element is an element that has any number of children, including zero. *)
  type ('a, 'b, 'c) star =
    ?a: (('a attrib) list) -> ('b elt) list_wrap -> 'c elt

  (** Various information about SVG, such as the doctype, ... *)
  module Info : Xml_sigs.Info

  (** {3 Uri} *)

  type uri = Xml.uri
  val string_of_uri : (uri, string) Xml.W.ft
  val uri_of_string : (string, uri) Xml.W.ft

  open Svg_types

  (** {2:attributes Attributes } *)

  val a_version : string wrap -> [> | `Version ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_baseProfile : string wrap -> [> | `BaseProfile ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_x : coord wrap -> [> | `X ] attrib

  val a_y : coord wrap -> [> | `Y ] attrib

  val a_width : Unit.length wrap -> [> | `Width ] attrib

  val a_height : Unit.length wrap -> [> | `Height ] attrib

  val a_preserveAspectRatio : string wrap -> [> | `PreserveAspectRatio ] attrib

  val a_crossorigin :
    [< | `Anonymous | `Use_credentials ] wrap -> [> | `Crossorigin ] attrib

  val a_decoding :
    [< | `Sync | `Async | `Auto ] wrap -> [> | `Decoding ] attrib

  val a_fetchpriority :
    [< | `High | `Low | `Auto ] wrap -> [> | `Fetchpriority ] attrib

  val a_contentScriptType : string wrap -> [> | `ContentScriptType ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_contentStyleType : string wrap -> [> | `ContentStyleType ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_zoomAndPan : [< | `Disable | `Magnify ] wrap -> [> | `ZoomAndPan ] attrib

  val a_href : iri wrap -> [> | `Xlink_href ] attrib

  val a_xlink_href : iri wrap -> [> | `Xlink_href ] attrib
    [@@ocaml.deprecated "Use a_href"]
  (** @deprecated Use a_href *)

  val a_requiredFeatures : spacestrings wrap -> [> | `RequiredFeatures ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_requiredExtensions :
    spacestrings wrap -> [> | `RequiredExtensions ] attrib

  val a_systemLanguage : commastrings wrap -> [> | `SystemLanguage ] attrib

  val a_externalResourcesRequired :
    bool wrap -> [> | `ExternalResourcesRequired ] attrib

  val a_id : string wrap -> [> | `Id ] attrib

  val a_user_data : string -> string wrap -> [> | `User_data] attrib

  val a_autofocus : unit -> [> | `Autofocus ] attrib
  (** Requests that the element be focused as soon as the document is
      loaded. *)

  val a_tabindex : int wrap -> [> | `Tabindex ] attrib
  (** Position of the element in the tabbing order.
      @see <https://www.w3.org/TR/SVG2/interact.html#TabindexAttribute>
      tabindex *)

  val a_lang : string wrap -> [> | `Lang ] attrib
  (** Language of the element content. It is a global attribute in SVG 2,
      whereas SVG 1.1 only allowed it on the [glyph] element. *)

  val a_xml_base : iri wrap -> [> | `Xml_base ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_xml_lang : string wrap -> [> | `Xml_lang ] attrib

  val a_xml_space : [< `Default | `Preserve ] wrap -> [> | `Xml_space ] attrib
    [@@ocaml.deprecated "Use CSS white-space"]
  (** @deprecated Use CSS white-space *)

  val a_type : string wrap -> [> | `Type ] attrib

  val a_media : commastrings wrap -> [> | `Media ] attrib

  val a_title : string wrap -> [> | `Title ] attrib

  val a_xlink_title : string wrap -> [> | `Xlink_title ] attrib

  val a_xlink_type : string wrap -> [> | `Xlink_type ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_xlink_role : iri wrap -> [> | `Xlink_role ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_xlink_arcrole : iri wrap -> [> | `Xlink_arcrole ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)
    [@@ocaml.deprecated "Use a child title element"]
  (** @deprecated Use a child title element *)

  val a_class : spacestrings wrap -> [> | `Class ] attrib

  val a_style : string wrap -> [> | `Style ] attrib

  (** {3 ARIA support} *)

  (** {{: https://www.w3.org/TR/wai-aria-1.1/} WAI-ARIA} attributes are part of
      SVG 2, and provide additional semantics to improve accessibility.

      @see <https://www.w3.org/TR/SVG2/struct.html#implicit-aria-semantics>
      Implicit ARIA semantics of SVG elements *)

  val a_role : string list wrap -> [> | `Role ] attrib
  (** @see <https://www.w3.org/TR/role-attribute> Role attribute specification
      @see <https://www.w3.org/TR/wai-aria-1.1/#role_definitions> List of
      WAI-ARIA roles *)

  val a_aria : string -> string list wrap -> [> | `Aria ] attrib
  (** Basic support for WAI-ARIA attributes: [a_aria "foo"] corresponds to an
      "aria-foo" attribute.

      @see <https://www.w3.org/TR/wai-aria-1.1/#state_prop_def> List of
      WAI-ARIA attributes *)

  val a_transform : transforms wrap -> [> | `Transform ] attrib

  val a_viewBox : fourfloats wrap -> [> | `ViewBox ] attrib

  val a_d : string wrap -> [> | `D ] attrib

  val a_pathLength : float wrap -> [> | `PathLength ] attrib

  (* XXX: better language support *)
  val a_rx : Unit.length wrap -> [> | `Rx ] attrib

  val a_ry : Unit.length wrap -> [> | `Ry ] attrib

  val a_cx : Unit.length wrap -> [> | `Cx ] attrib

  val a_cy : Unit.length wrap -> [> | `Cy ] attrib

  val a_r : Unit.length wrap -> [> | `R ] attrib

  val a_x1 : coord wrap -> [> | `X1 ] attrib

  val a_y1 : coord wrap -> [> | `Y1 ] attrib

  val a_x2 : coord wrap -> [> | `X2 ] attrib

  val a_y2 : coord wrap -> [> | `Y2 ] attrib

  val a_points : coords wrap -> [> | `Points ] attrib

  val a_x_list : lengths wrap -> [> | `X_list ] attrib
    [@@reflect.attribute "x" ["text"; "tspan"; "tref"; "altGlyph"]]

  val a_y_list : lengths wrap -> [> | `Y_list ] attrib
    [@@reflect.attribute "y" ["text"; "tspan"; "tref"; "altGlyph"]]

  val a_dx : number wrap -> [> | `Dx ] attrib

  val a_dy : number wrap -> [> | `Dy ] attrib

  val a_dx_list : lengths wrap -> [> | `Dx_list ] attrib
    [@@reflect.attribute "dx" ["text"; "tspan"; "tref"; "altGlyph"]]

  val a_dy_list : lengths wrap -> [> | `Dy_list ] attrib
    [@@reflect.attribute "dy" ["text"; "tspan"; "tref"; "altGlyph"]]

  val a_lengthAdjust :
    [< `Spacing | `SpacingAndGlyphs ] wrap -> [> | `LengthAdjust ] attrib

  val a_textLength : Unit.length wrap -> [> | `TextLength ] attrib

  val a_text_anchor : [< `Start | `Middle | `End | `Inherit ] wrap -> [> | `Text_Anchor ] attrib

  val a_text_decoration : [< `None | `Underline | `Overline | `Line_through | `Blink | `Inherit ] wrap -> [> | `Text_Decoration ] attrib

  val a_text_rendering : [< `Auto | `OptimizeSpeed | `OptimizeLegibility | `GeometricPrecision | `Inherit ] wrap -> [> | `Text_Rendering ] attrib

  val a_rotate : numbers wrap -> [> | `Rotate ] attrib

  val a_startOffset : Unit.length wrap -> [> | `StartOffset ] attrib

  val a_method : [< `Align | `Stretch ] wrap -> [> | `Method ] attrib

  val a_spacing : [< `Auto | `Exact ] wrap -> [> | `Spacing ] attrib

  val a_side : [< `Left | `Right ] wrap -> [> | `Side ] attrib
  (** Side of the path a [textPath] is rendered on. New in SVG 2. *)

  val a_glyphRef : string wrap -> [> | `GlyphRef ] attrib

  val a_format : string wrap -> [> | `Format ] attrib

  val a_markerUnits :
    [< `StrokeWidth | `UserSpaceOnUse ] wrap -> [> | `MarkerUnits ] attrib

  val a_refX : coord wrap -> [> | `RefX ] attrib

  val a_refY : coord wrap -> [> | `RefY ] attrib

  val a_markerWidth : Unit.length wrap -> [> | `MarkerWidth ] attrib

  val a_markerHeight : Unit.length wrap -> [> | `MarkerHeight ] attrib

  val a_orient : Unit.angle option wrap -> [> | `Orient ] attrib

  val a_local : string wrap -> [> | `Local ] attrib

  val a_rendering_intent :
    [<
      | `Auto
      | `Perceptual
      | `Relative_colorimetric
      | `Saturation
      | `Absolute_colorimetric ] wrap -> [> | `Rendering_Intent ] attrib

  val a_gradientUnits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
    [ | `GradientUnits ] attrib

  val a_gradientTransform : transforms wrap -> [> | `GradientTransform ] attrib

  val a_spreadMethod :
    [< `Pad | `Reflect | `Repeat ] wrap -> [> | `SpreadMethod ] attrib

  val a_fx : coord wrap -> [> | `Fx ] attrib

  val a_fy : coord wrap -> [> | `Fy ] attrib

  val a_fr : Unit.length wrap -> [> | `Fr ] attrib
  (** Radius of the focal circle of a radial gradient. New in SVG 2. *)

  val a_offset :
    [< `Number of number | `Percentage of percentage ] wrap ->
    [> | `Offset ] attrib

  val a_patternUnits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
    [> | `PatternUnits ] attrib

  val a_patternContentUnits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
    [> | `PatternContentUnits ] attrib

  val a_patternTransform : transforms wrap -> [> | `PatternTransform ] attrib

  val a_clip_path : iri wrap -> [> | `Clip_Path ] attrib

  val a_clipPathUnits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
    [> | `ClipPathUnits ] attrib

  val a_maskUnits :
    [< | `UserSpaceOnUse | `ObjectBoundingBox ] wrap -> [> | `MaskUnits ] attrib

  val a_maskContentUnits :
    [< | `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
    [> | `MaskContentUnits ] attrib

  val a_filterUnits :
    [< | `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
    [> | `FilterUnits ] attrib

  val a_primitiveUnits :
    [< | `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
    [> | `PrimitiveUnits ] attrib

  val a_filterRes : number_optional_number wrap -> [> | `FilterRes ] attrib

  val a_result : string wrap -> [> | `Result ] attrib

  val a_in :
    [<
      | `SourceGraphic
      | `SourceAlpha
      | `BackgroundImage
      | `BackgroundAlpha
      | `FillPaint
      | `StrokePaint
      | `Ref of string ] wrap -> [> | `In ] attrib

  val a_in2 :
    [<
      | `SourceGraphic
      | `SourceAlpha
      | `BackgroundImage
      | `BackgroundAlpha
      | `FillPaint
      | `StrokePaint
      | `Ref of string ] wrap -> [> | `In2 ] attrib

  val a_azimuth : float wrap -> [> | `Azimuth ] attrib

  val a_elevation : float wrap -> [> | `Elevation ] attrib

  val a_z : number wrap -> [> | `Z ] attrib
  (** The [z] coordinate of a light source. *)

  val a_pointsAtX : float wrap -> [> | `PointsAtX ] attrib

  val a_pointsAtY : float wrap -> [> | `PointsAtY ] attrib

  val a_pointsAtZ : float wrap -> [> | `PointsAtZ ] attrib

  val a_specularExponent : float wrap -> [> | `SpecularExponent ] attrib

  val a_specularConstant : float wrap -> [> | `SpecularConstant ] attrib

  val a_limitingConeAngle : float wrap -> [> | `LimitingConeAngle ] attrib

  val a_mode :
    [< | `Normal | `Multiply | `Screen | `Overlay | `Darken | `Lighten
       | `Color_dodge | `Color_burn | `Hard_light | `Soft_light | `Difference
       | `Exclusion | `Hue | `Saturation | `Color | `Luminosity ] wrap ->
    [> | `Mode ] attrib
  (** The blend mode of [feBlend]. SVG 2 allows every CSS compositing blend
      mode, not only the five modes of SVG 1.1. *)

  val a_feColorMatrix_type :
    [< | `Matrix | `Saturate | `HueRotate | `LuminanceToAlpha ] wrap ->
    [> | `Typefecolor ] attrib
    [@@reflect.attribute "type" ["feColorMatrix"]]

  val a_values : numbers wrap -> [> | `Values ] attrib

  val a_transfer_type :
    [< | `Identity | `Table | `Discrete | `Linear | `Gamma ] wrap ->
    [> | `Type_transfert ] attrib
    [@@reflect.attribute "type" ["feFuncR"; "feFuncG"; "feFuncB"; "feFuncA"]]

  val a_tableValues : numbers wrap -> [> | `TableValues ] attrib

  val a_intercept : number wrap -> [> | `Intercept ] attrib

  val a_amplitude : number wrap -> [> | `Amplitude ] attrib

  val a_exponent : number wrap -> [> | `Exponent ] attrib

  val a_transfer_offset : number wrap -> [> | `Offset_transfer ] attrib
    [@@reflect.attribute "offset" ["feFuncR"; "feFuncG"; "feFuncB"; "feFuncA"]]

  val a_feComposite_operator :
    [< | `Over | `In | `Out | `Atop | `Xor | `Arithmetic ] wrap ->
    [> | `OperatorComposite ] attrib
    [@@reflect.attribute "operator" ["feComposite"]]

  val a_k1 : number wrap -> [> | `K1 ] attrib

  val a_k2 : number wrap -> [> | `K2 ] attrib

  val a_k3 : number wrap -> [> | `K3 ] attrib

  val a_k4 : number wrap -> [> | `K4 ] attrib

  val a_order : number_optional_number wrap -> [> | `Order ] attrib

  val a_kernelMatrix : numbers wrap -> [> | `KernelMatrix ] attrib

  val a_divisor : number wrap -> [> | `Divisor ] attrib

  val a_bias : number wrap -> [> | `Bias ] attrib

  val a_kernelUnitLength :
    number_optional_number wrap -> [> | `KernelUnitLength ] attrib

  val a_targetX : int wrap -> [> | `TargetX ] attrib

  val a_targetY : int wrap -> [> | `TargetY ] attrib

  val a_edgeMode :
    [< | `Duplicate | `Wrap | `None ] wrap -> [> | `EdgeMode ] attrib

  val a_preserveAlpha : bool wrap -> [> | `PreserveAlpha ] attrib

  val a_surfaceScale : number wrap -> [> | `SurfaceScale ] attrib

  val a_diffuseConstant : number wrap -> [> | `DiffuseConstant ] attrib

  val a_scale : number wrap -> [> | `Scale ] attrib

  val a_xChannelSelector :
    [< | `R | `G | `B | `A ] wrap -> [> | `XChannelSelector ] attrib

  val a_yChannelSelector :
    [< | `R | `G | `B | `A ] wrap -> [> | `YChannelSelector ] attrib

  val a_stdDeviation : number_optional_number wrap -> [> | `StdDeviation ] attrib

  val a_feMorphology_operator :
    [< | `Erode | `Dilate ] wrap -> [> | `OperatorMorphology ] attrib
    [@@reflect.attribute "operator" ["feMorphology"]]

  val a_radius : number_optional_number wrap -> [> | `Radius ] attrib

  val a_baseFrequency :
    number_optional_number wrap -> [> | `BaseFrequency ] attrib

  val a_numOctaves : int wrap -> [> | `NumOctaves ] attrib

  val a_seed : number wrap -> [> | `Seed ] attrib

  val a_stitchTiles :
    [< | `Stitch | `NoStitch ] wrap -> [> | `StitchTiles ] attrib

  val a_feTurbulence_type :
    [< | `FractalNoise | `Turbulence ] wrap -> [> | `TypeStitch ] attrib
    [@@reflect.attribute "type" ["feTurbulence"]]

  val a_xlink_show : [< | `New | `Replace ] wrap -> [> | `Xlink_show ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_xlink_actuate :
    [< | `OnRequest | `OnLoad | `Other | `None ] wrap
    -> [> | `Xlink_actuate ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_target : string wrap -> [> | `Target ] attrib

  (** {3 Link attributes}

      These attributes apply to the [a] element, as in HTML. *)

  val a_download : string option wrap -> [> | `Download ] attrib
  (** [None] downloads the resource under its default name. *)

  val a_hreflang : string wrap -> [> | `Hreflang ] attrib

  val a_ping : spacestrings wrap -> [> | `Ping ] attrib
  (** Space separated list of URLs to ping when the link is followed. *)

  val a_referrerpolicy :
    [< | `No_referrer | `No_referrer_when_downgrade | `Origin
       | `Origin_when_cross_origin | `Same_origin | `Strict_origin
       | `Strict_origin_when_cross_origin | `Unsafe_url ] wrap ->
    [> | `Referrerpolicy ] attrib

  val a_rel : spacestrings wrap -> [> | `Rel ] attrib
  (** Space separated list of link types describing the relationship between
      the current document and the linked resource. *)

  val a_viewTarget : string wrap -> [> | `ViewTarget ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_attributeName : string wrap -> [> | `AttributeName ] attrib

  val a_attributeType :
    [< | `CSS | `XML | `Auto ] wrap -> [> | `AttributeType ] attrib

  val a_begin : string wrap -> [> | `Begin ] attrib

  val a_dur : string wrap -> [> | `Dur ] attrib

  val a_end : string wrap -> [> | `End ] attrib

  val a_min : string wrap -> [> | `Min ] attrib

  val a_max : string wrap -> [> | `Max ] attrib

  val a_restart :
    [< | `Always | `WhenNotActive | `Never ] wrap -> [> | `Restart ] attrib

  val a_repeatCount : string wrap -> [> | `RepeatCount ] attrib

  val a_repeatDur : string wrap -> [> | `RepeatDur ] attrib

  val a_fill : paint wrap -> [> | `Fill ] attrib

  val a_animation_fill : [< | `Freeze | `Remove ] wrap -> [> | `Fill_Animation ] attrib
    [@@reflect.attribute "fill" ["animate"]]

  val a_fill_rule : fill_rule wrap -> [> | `Fill_rule ] attrib

  val a_calcMode :
    [< | `Discrete | `Linear | `Paced | `Spline ] wrap -> [> | `CalcMode ] attrib

  val a_animation_values : strings wrap -> [> | `Valuesanim ] attrib
    [@@reflect.attribute "values" ["animate"]]

  val a_keyTimes : strings wrap -> [> | `KeyTimes ] attrib

  val a_keySplines : strings wrap -> [> | `KeySplines ] attrib

  val a_from : string wrap -> [> | `From ] attrib

  val a_to : string wrap -> [> | `To ] attrib

  val a_by : string wrap -> [> | `By ] attrib

  val a_additive : [< | `Replace | `Sum ] wrap -> [> | `Additive ] attrib

  val a_accumulate : [< | `None | `Sum ] wrap -> [> | `Accumulate ] attrib

  val a_keyPoints : numbers_semicolon wrap -> [> | `KeyPoints ] attrib

  val a_origin : string wrap -> [> | `Origin ] attrib

  val a_path : string wrap -> [> | `Path ] attrib

  val a_animateTransform_type :
    [ | `Translate | `Scale | `Rotate | `SkewX | `SkewY ] wrap ->
    [ | `Typeanimatetransform ] attrib
    [@@reflect.attribute "type" ["animateTransform"]]

  val a_horiz_origin_x : number wrap -> [> | `HorizOriginX ] attrib

  val a_horiz_origin_y : number wrap -> [> | `HorizOriginY ] attrib

  val a_horiz_adv_x : number wrap -> [> | `HorizAdvX ] attrib

  val a_vert_origin_x : number wrap -> [> | `VertOriginX ] attrib

  val a_vert_origin_y : number wrap -> [> | `VertOriginY ] attrib

  val a_vert_adv_y : number wrap -> [> | `VertAdvY ] attrib

  val a_unicode : string wrap -> [> | `Unicode ] attrib

  val a_glyph_name : string wrap -> [> | `GlyphName ] attrib

  val a_orientation : [< | `H | `V ] wrap -> [> | `Orientation ] attrib

  val a_arabic_form :
    [< | `Initial | `Medial | `Terminal | `Isolated ] wrap ->
    [> | `ArabicForm ] attrib

  val a_u1 : string wrap -> [> | `U1 ] attrib

  val a_u2 : string wrap -> [> | `U2 ] attrib

  val a_g1 : string wrap -> [> | `G1 ] attrib

  val a_g2 : string wrap -> [> | `G2 ] attrib

  val a_k : string wrap -> [> | `K ] attrib

  val a_font_family : string wrap -> [> | `Font_Family ] attrib

  val a_font_style : string wrap -> [> | `Font_Style ] attrib

  val a_font_variant : string wrap -> [> | `Font_Variant ] attrib

  val a_font_weight : string wrap -> [> | `Font_Weight ] attrib

  val a_font_stretch : string wrap -> [> | `Font_Stretch ] attrib

  val a_font_size : string wrap -> [> | `Font_Size ] attrib

  val a_unicode_range : string wrap -> [> | `UnicodeRange ] attrib

  val a_units_per_em : string wrap -> [> | `UnitsPerEm ] attrib

  val a_panose_1 : string wrap -> [> | `Panose1 ] attrib

  val a_stemv : number wrap -> [> | `Stemv ] attrib

  val a_stemh : number wrap -> [> | `Stemh ] attrib

  val a_slope : number wrap -> [> | `Slope ] attrib

  val a_cap_height : number wrap -> [> | `CapHeight ] attrib

  val a_x_height : number wrap -> [> | `XHeight ] attrib

  val a_accent_height : number wrap -> [> | `AccentHeight ] attrib

  val a_ascent : number wrap -> [> | `Ascent ] attrib

  val a_descent : number wrap -> [> | `Descent ] attrib

  val a_widths : string wrap -> [> | `Widths ] attrib

  val a_bbox : string wrap -> [> | `Bbox ] attrib

  val a_ideographic : number wrap -> [> | `Ideographic ] attrib

  val a_alphabetic : number wrap -> [> | `Alphabetic ] attrib

  val a_mathematical : number wrap -> [> | `Mathematical ] attrib

  val a_hanging : number wrap -> [> | `Hanging ] attrib

  val a_videographic : number wrap -> [> | `VIdeographic ] attrib

  val a_v_alphabetic : number wrap -> [> | `VAlphabetic ] attrib

  val a_v_mathematical : number wrap -> [> | `VMathematical ] attrib

  val a_v_hanging : number wrap -> [> | `VHanging ] attrib

  val a_underline_position : number wrap -> [> | `UnderlinePosition ] attrib

  val a_underline_thickness : number wrap -> [> | `UnderlineThickness ] attrib

  val a_strikethrough_position :
    number wrap -> [> | `StrikethroughPosition ] attrib

  val a_strikethrough_thickness :
    number wrap -> [> | `StrikethroughThickness ] attrib

  val a_overline_position : number wrap -> [> | `OverlinePosition ] attrib

  val a_overline_thickness : number wrap -> [> | `OverlineThickness ] attrib

  val a_string : string wrap -> [> | `String ] attrib

  val a_name : string wrap -> [> | `Name ] attrib

  val a_alignment_baseline :
    [< | `Auto | `Baseline | `Before_edge | `Text_before_edge | `Middle
       | `Central | `After_edge | `Text_after_edge | `Ideographic
       | `Alphabetic | `Hanging | `Mathematical | `Text_bottom | `Text_top
       | `Inherit ] wrap ->
    [> | `Alignment_Baseline ] attrib

  val a_dominant_baseline :
    [< | `Auto | `Use_script | `No_change | `Reset_size | `Ideographic
       | `Alphabetic | `Hanging | `Mathematical | `Central | `Middle
       | `Text_after_edge | `Text_before_edge | `Text_bottom | `Text_top
       | `Inherit ] wrap ->
    [> | `Dominant_Baseline ] attrib

  val a_stop_color : color wrap -> [> | `Stop_Color ] attrib

  val a_stop_opacity : number wrap -> [> | `Stop_Opacity ] attrib

  val a_stroke : paint wrap -> [> | `Stroke ] attrib

  val a_stroke_width : Unit.length wrap -> [> | `Stroke_Width ] attrib

  val a_stroke_linecap :
    [< `Butt | `Round | `Square ] wrap -> [> | `Stroke_Linecap ] attrib

  val a_stroke_linejoin :
    [< `Miter | `Miter_clip | `Round | `Bevel | `Arcs ] wrap ->
    [> `Stroke_Linejoin ] attrib

  val a_stroke_miterlimit : float wrap -> [> `Stroke_Miterlimit ] attrib

  val a_stroke_dasharray :
    Unit.length list wrap -> [> `Stroke_Dasharray ] attrib

  val a_stroke_dashoffset : Unit.length wrap -> [> `Stroke_Dashoffset ] attrib

  val a_stroke_opacity : float wrap -> [> `Stroke_Opacity ] attrib

  val a_baseline_shift : string wrap -> [> | `Baseline_Shift ] attrib

  val a_clip : string wrap -> [> | `Clip ] attrib
    [@@ocaml.deprecated "Deprecated in SVG2, use a_clip_path"]
  (** @deprecated Deprecated in SVG2, use {!a_clip_path} *)

  val a_clip_rule :
    [< | `Nonzero | `Evenodd | `Inherit ] wrap -> [> | `Clip_Rule ] attrib

  val a_color : color wrap -> [> | `Color ] attrib

  val a_color_interpolation :
    [< | `Auto | `SRGB | `LinearRGB | `Inherit ] wrap ->
    [> | `Color_Interpolation ] attrib

  val a_color_interpolation_filters :
    [< | `Auto | `SRGB | `LinearRGB | `Inherit ] wrap ->
    [> | `Color_interpolation_filters ] attrib

  val a_color_profile : string wrap -> [> | `Color_profile ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_color_rendering :
    [< | `Auto | `OptimizeSpeed | `OptimizeQuality | `Inherit ] wrap ->
    [> | `Color_rendering ] attrib

  val a_cursor : string wrap -> [> | `Cursor ] attrib

  val a_direction :
    [< | `Ltr | `Rtl | `Inherit ] wrap -> [> | `Direction ] attrib

  val a_display : string wrap -> [> | `Display ] attrib

  val a_enable_background :
    [< | `Accumulate | `New | `Inherit ] wrap ->
    [> | `Enable_background ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_fill_opacity : number wrap -> [> | `Fill_opacity ] attrib

  val a_filter : string wrap -> [> | `Filter ] attrib

  val a_flood_color : color wrap -> [> | `Flood_Color ] attrib

  val a_flood_opacity : number wrap -> [> | `Flood_Opacity ] attrib

  val a_font_size_adjust : string wrap -> [> | `Font_Size_Adjust ] attrib

  val a_glyph_orientation_horizontal :
    string wrap -> [> | `Glyph_Orientation_Horizontal ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_glyph_orientation_vertical :
    string wrap -> [> | `Glyph_Orientation_Vertical ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_image_rendering :
    [< | `Auto | `OptimizeSpeed | `OptimizeQuality | `Inherit ] wrap ->
    [> | `Image_Rendering ] attrib

  val a_kerning : string wrap -> [> | `Kerning ] attrib
    [@@ocaml.deprecated "Removed in SVG2, use CSS font-kerning"]
  (** @deprecated Removed in SVG2, use CSS font-kerning *)

  val a_letter_spacing : string wrap -> [> | `Letter_Spacing ] attrib

  val a_lighting_color : color wrap -> [> | `Lighting_Color ] attrib

  val a_marker_end : iri wrap -> [> | `Marker_End ] attrib

  val a_marker_mid : iri wrap -> [> | `Marker_Mid ] attrib

  val a_marker_start : iri wrap -> [> | `Marker_Start ] attrib

  val a_mask : iri wrap -> [> | `Mask ] attrib

  val a_opacity : number wrap -> [> | `Opacity ] attrib

  val a_overflow :
    [< | `Visible | `Hidden | `Scroll | `Auto | `Inherit ] wrap ->
    [> | `Overflow ] attrib

  val a_pointer_events :
    [< | `VisiblePainted | `VisibleFill | `VisibleStroke | `Visible
       | `Painted | `Fill | `Stroke | `All | `None | `Inherit ] wrap ->
    [> | `Pointer_Events ] attrib

  val a_shape_rendering :
    [< | `Auto | `OptimizeSpeed | `CrispEdges | `GeometricPrecision
       | `Inherit ] wrap -> [> | `Shape_Rendering ] attrib

  val a_unicode_bidi :
    [< | `Normal | `Embed | `Bidi_override | `Inherit ] wrap ->
    [> | `Unicode_Bidi ] attrib

  val a_visibility :
    [< | `Visible | `Hidden | `Collapse | `Inherit ] wrap ->
    [> | `Visibility ] attrib

  val a_word_spacing : string wrap -> [> | `Word_Spacing ] attrib

  val a_writing_mode :
    [< | `Horizontal_tb | `Vertical_rl | `Vertical_lr | `Inherit ] wrap ->
    [> | `Writing_Mode ] attrib

  (** {3 Presentation attributes new in SVG 2} *)

  val a_paint_order : string wrap -> [> | `Paint_Order ] attrib
  (** Order in which the fill, stroke and markers of a shape are painted.
      @see <https://www.w3.org/TR/SVG2/painting.html#PaintOrder> paint-order *)

  val a_text_overflow :
    [< | `Clip | `Ellipsis ] wrap -> [> | `Text_Overflow ] attrib

  val a_transform_origin : string wrap -> [> | `Transform_Origin ] attrib

  val a_vector_effect :
    [< | `None | `Non_scaling_stroke | `Non_scaling_size | `Non_rotation
       | `Fixed_position ] wrap -> [> | `Vector_Effect ] attrib
  (** @see <https://www.w3.org/TR/SVG2/coords.html#VectorEffects>
      vector-effect *)

  val a_white_space :
    [< | `Normal | `Pre | `Nowrap | `Pre_wrap | `Break_spaces | `Pre_line ]
      wrap -> [> | `White_Space ] attrib
  (** Replaces the deprecated [xml:space] attribute. *)

  (** {2 Events}

      {3 Javascript events} *)

  val a_onabort : Xml.event_handler  -> [> | `OnAbort ] attrib
  val a_onactivate : Xml.event_handler  -> [> | `OnActivate ] attrib
  val a_onbegin : Xml.event_handler  -> [> | `OnBegin ] attrib
  val a_onend : Xml.event_handler  -> [> | `OnEnd ] attrib
  val a_onerror : Xml.event_handler  -> [> | `OnError ] attrib
  val a_onfocusin : Xml.event_handler  -> [> | `OnFocusIn ] attrib
  val a_onfocusout : Xml.event_handler  -> [> | `OnFocusOut ] attrib
  val a_onload : Xml.event_handler  -> [> | `OnLoad ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_onrepeat : Xml.event_handler  -> [> | `OnRepeat ] attrib
  val a_onresize : Xml.event_handler  -> [> | `OnResize ] attrib
  val a_onscroll : Xml.event_handler  -> [> | `OnScroll ] attrib
  val a_onunload : Xml.event_handler  -> [> | `OnUnload ] attrib
  val a_onzoom : Xml.event_handler  -> [> | `OnZoom ] attrib

  (** {3 Javascript document events}

      These apply to the outermost [svg] element only. *)

  val a_onafterprint : Xml.event_handler -> [> | `OnAfterPrint ] attrib
  val a_onbeforeprint : Xml.event_handler -> [> | `OnBeforePrint ] attrib
  val a_onbeforeunload : Xml.event_handler -> [> | `OnBeforeUnload ] attrib
  val a_onhashchange : Xml.event_handler -> [> | `OnHashChange ] attrib
  val a_onlanguagechange :
    Xml.event_handler -> [> | `OnLanguageChange ] attrib
  val a_onmessage : Xml.event_handler -> [> | `OnMessage ] attrib
  val a_onoffline : Xml.event_handler -> [> | `OnOffLine ] attrib
  val a_ononline : Xml.event_handler -> [> | `OnOnLine ] attrib
  val a_onpagehide : Xml.event_handler -> [> | `OnPageHide ] attrib
  val a_onpageshow : Xml.event_handler -> [> | `OnPageShow ] attrib
  val a_onpopstate : Xml.event_handler -> [> | `OnPopState ] attrib
  val a_onreadystatechange :
    Xml.event_handler -> [> | `OnReadyStateChange ] attrib
  val a_onredo : Xml.event_handler -> [> | `OnRedo ] attrib
  val a_onrejectionhandled :
    Xml.event_handler -> [> | `OnRejectionHandled ] attrib
  val a_onstorage : Xml.event_handler -> [> | `OnStorage ] attrib
  val a_onundo : Xml.event_handler -> [> | `OnUndo ] attrib
  val a_onunhandledrejection :
    Xml.event_handler -> [> | `OnUnhandledRejection ] attrib

  (** {3 Javascript global events}

      SVG 2 reuses the event handler content attributes of HTML;
      these apply to every SVG element. *)

  val a_onauxclick : Xml.mouse_event_handler -> [> | `OnAuxClick ] attrib
  val a_onbeforeinput : Xml.event_handler -> [> | `OnBeforeInput ] attrib
  val a_onbeforematch : Xml.event_handler -> [> | `OnBeforeMatch ] attrib
  val a_onbeforetoggle : Xml.event_handler -> [> | `OnBeforeToggle ] attrib
  val a_onblur : Xml.event_handler -> [> | `OnBlur ] attrib
  val a_oncancel : Xml.event_handler -> [> | `OnCancel ] attrib
  val a_oncanplay : Xml.event_handler -> [> | `OnCanPlay ] attrib
  val a_oncanplaythrough :
    Xml.event_handler -> [> | `OnCanPlayThrough ] attrib
  val a_onchange : Xml.event_handler -> [> | `OnChange ] attrib
  val a_onclose : Xml.event_handler -> [> | `OnClose ] attrib
  val a_oncontextlost : Xml.event_handler -> [> | `OnContextLost ] attrib
  val a_oncontextmenu :
    Xml.mouse_event_handler -> [> | `OnContextMenu ] attrib
  val a_oncontextrestored :
    Xml.event_handler -> [> | `OnContextRestored ] attrib
  val a_oncopy : Xml.event_handler -> [> | `OnCopy ] attrib
  val a_oncuechange : Xml.event_handler -> [> | `OnCueChange ] attrib
  val a_oncut : Xml.event_handler -> [> | `OnCut ] attrib
  val a_ondblclick : Xml.mouse_event_handler -> [> | `OnDblClick ] attrib
  val a_ondrag : Xml.mouse_event_handler -> [> | `OnDrag ] attrib
  val a_ondragend : Xml.mouse_event_handler -> [> | `OnDragEnd ] attrib
  val a_ondragenter : Xml.mouse_event_handler -> [> | `OnDragEnter ] attrib
  val a_ondragleave : Xml.mouse_event_handler -> [> | `OnDragLeave ] attrib
  val a_ondragover : Xml.mouse_event_handler -> [> | `OnDragOver ] attrib
  val a_ondragstart : Xml.mouse_event_handler -> [> | `OnDragStart ] attrib
  val a_ondrop : Xml.mouse_event_handler -> [> | `OnDrop ] attrib
  val a_ondurationchange :
    Xml.event_handler -> [> | `OnDurationChange ] attrib
  val a_onemptied : Xml.event_handler -> [> | `OnEmptied ] attrib
  val a_onended : Xml.event_handler -> [> | `OnEnded ] attrib
  val a_onfocus : Xml.event_handler -> [> | `OnFocus ] attrib
  val a_ongotpointercapture :
    Xml.mouse_event_handler -> [> | `OnGotPointerCapture ] attrib
  val a_oninput : Xml.event_handler -> [> | `OnInput ] attrib
  val a_oninvalid : Xml.event_handler -> [> | `OnInvalid ] attrib
  val a_onkeydown : Xml.keyboard_event_handler -> [> | `OnKeyDown ] attrib
  val a_onkeypress : Xml.keyboard_event_handler -> [> | `OnKeyPress ] attrib
  val a_onkeyup : Xml.keyboard_event_handler -> [> | `OnKeyUp ] attrib
  val a_onloadeddata : Xml.event_handler -> [> | `OnLoadedData ] attrib
  val a_onloadedmetadata :
    Xml.event_handler -> [> | `OnLoadedMetaData ] attrib
  val a_onloadstart : Xml.event_handler -> [> | `OnLoadStart ] attrib
  val a_onlostpointercapture :
    Xml.mouse_event_handler -> [> | `OnLostPointerCapture ] attrib
  val a_onmousewheel : Xml.event_handler -> [> | `OnMouseWheel ] attrib
  val a_onpaste : Xml.event_handler -> [> | `OnPaste ] attrib
  val a_onpause : Xml.event_handler -> [> | `OnPause ] attrib
  val a_onplay : Xml.event_handler -> [> | `OnPlay ] attrib
  val a_onplaying : Xml.event_handler -> [> | `OnPlaying ] attrib
  val a_onpointercancel :
    Xml.mouse_event_handler -> [> | `OnPointerCancel ] attrib
  val a_onpointerdown :
    Xml.mouse_event_handler -> [> | `OnPointerDown ] attrib
  val a_onpointerenter :
    Xml.mouse_event_handler -> [> | `OnPointerEnter ] attrib
  val a_onpointerleave :
    Xml.mouse_event_handler -> [> | `OnPointerLeave ] attrib
  val a_onpointermove :
    Xml.mouse_event_handler -> [> | `OnPointerMove ] attrib
  val a_onpointerout : Xml.mouse_event_handler -> [> | `OnPointerOut ] attrib
  val a_onpointerover :
    Xml.mouse_event_handler -> [> | `OnPointerOver ] attrib
  val a_onpointerup : Xml.mouse_event_handler -> [> | `OnPointerUp ] attrib
  val a_onprogress : Xml.event_handler -> [> | `OnProgress ] attrib
  val a_onratechange : Xml.event_handler -> [> | `OnRateChange ] attrib
  val a_onscrollend : Xml.event_handler -> [> | `OnScrollEnd ] attrib
  val a_onsecuritypolicyviolation :
    Xml.event_handler -> [> | `OnSecurityPolicyViolation ] attrib
  val a_onseeked : Xml.event_handler -> [> | `OnSeeked ] attrib
  val a_onseeking : Xml.event_handler -> [> | `OnSeeking ] attrib
  val a_onselect : Xml.event_handler -> [> | `OnSelect ] attrib
  val a_onshow : Xml.event_handler -> [> | `OnShow ] attrib
  val a_onstalled : Xml.event_handler -> [> | `OnStalled ] attrib
  val a_onsubmit : Xml.event_handler -> [> | `OnSubmit ] attrib
  val a_onsuspend : Xml.event_handler -> [> | `OnSuspend ] attrib
  val a_ontimeupdate : Xml.event_handler -> [> | `OnTimeUpdate ] attrib
  val a_ontoggle : Xml.event_handler -> [> | `OnToggle ] attrib
  val a_onvolumechange : Xml.event_handler -> [> | `OnVolumeChange ] attrib
  val a_onwaiting : Xml.event_handler -> [> | `OnWaiting ] attrib
  val a_onwheel : Xml.mouse_event_handler -> [> | `OnWheel ] attrib

  (** {3 Javascript mouse events} *)

  val a_onclick : Xml.mouse_event_handler  -> [> | `OnClick ] attrib
  val a_onmousedown : Xml.mouse_event_handler  -> [> | `OnMouseDown ] attrib
  val a_onmouseup : Xml.mouse_event_handler  -> [> | `OnMouseUp ] attrib
  val a_onmouseover : Xml.mouse_event_handler  -> [> | `OnMouseOver ] attrib
  val a_onmouseout : Xml.mouse_event_handler  -> [> | `OnMouseOut ] attrib
  val a_onmousemove : Xml.mouse_event_handler  -> [> | `OnMouseMove ] attrib

  (** {3 Javascript touch events} *)
  val a_ontouchstart : Xml.touch_event_handler -> [> | `OnTouchStart] attrib
  val a_ontouchend : Xml.touch_event_handler -> [> | `OnTouchEnd] attrib
  val a_ontouchmove : Xml.touch_event_handler -> [> | `OnTouchMove] attrib
  val a_ontouchcancel : Xml.touch_event_handler -> [> | `OnTouchCancel] attrib

  (** {2:elements Elements} *)

  val txt : string wrap -> [> | txt] elt

  val svg : ([< | svg_attr], [< | svg_content], [> | svg]) star

  val g : ([< | g_attr], [< | g_content], [> | g]) star

  val defs : ([< | defs_attr], [< | defs_content], [> | defs]) star

  val desc : ([< | desc_attr], [< | desc_content], [> | desc]) unary

  val title : ([< | title_attr], [< | title_content], [> | title]) unary

  val symbol : ([< | symbol_attr], [< | symbol_content], [> | symbol]) star

  val use : ([< | use_attr], [< | use_content], [> | use]) star

  val image : ([< | image_attr], [< | image_content], [> | image]) star

  val switch : ([< | switch_attr], [< | switch_content], [> | switch]) star

  val style : ([< | style_attr], [< | style_content], [> | style]) unary

  val path : ([< | path_attr], [< | path_content], [> | path]) star

  val rect : ([< | rect_attr], [< | rect_content], [> | rect]) star

  val circle : ([< | circle_attr], [< | circle_content], [> | circle]) star

  val ellipse :
    ([< | ellipse_attr], [< | ellipse_content], [> | ellipse]) star

  val line : ([< | line_attr], [< | line_content], [> | line]) star

  val polyline :
    ([< | polyline_attr], [< | polyline_content], [> | polyline]) star

  val polygon :
    ([< | polygon_attr], [< | polygon_content], [> | polygon]) star

  val text : ([< | text_attr], [< | text_content], [> | text]) star

  val tspan : ([< | tspan_attr], [< | tspan_content], [> | tspan]) star

  val tref : ([< | tref_attr], [< | tref_content], [> | tref]) star
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val textPath :
    ([< | textpath_attr], [< | textpath_content], [> | textpath]) star

  val altGlyph :
    ([< | altglyph_attr], [< | altglyph_content], [> | altglyph]) unary
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  type altglyphdef_content =
    [ | `Ref of (glyphref elt) list | `Item of (altglyphitem elt) list
    ]

  val altGlyphDef :
    ([< | altglyphdef_attr], [< | altglyphdef_content], [> | altglyphdef])
      unary
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val altGlyphItem :
    ([< | altglyphitem_attr], [< | altglyphitem_content], [> | altglyphitem
                                                          ]) star
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val glyphRef : ([< | glyphref_attr], [> | glyphref]) nullary
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val marker : ([< | marker_attr], [< | marker_content], [> | marker]) star

  val color_profile :
    ([< | colorprofile_attr], [< | colorprofile_content], [> | colorprofile
                                                          ]) star
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val linearGradient :
    ([< | lineargradient_attr], [< | lineargradient_content],
     [> | lineargradient]) star

  val radialGradient :
    ([< | radialgradient_attr], [< | radialgradient_content],
     [> | radialgradient]) star

  val stop :
    ([< | stop_attr], [< | stop_content], [> | stop ]) star

  val pattern :
    ([< | pattern_attr], [< | pattern_content], [> | pattern]) star

  val clipPath :
    ([< | clippath_attr], [< | clippath_content], [> | clippath]) star

  val mask : ([< | mask_attr], [< | mask_content], [> | mask]) star

  val filter : ([< | filter_attr], [< | filter_content], [> | filter]) star

  val feDistantLight :
    ([< | fedistantlight_attr], [< | fedistantlight_content],
     [> | fedistantlight]) star

  val fePointLight :
    ([< | fepointlight_attr], [< | fepointlight_content], [> | fepointlight
                                                          ]) star

  val feSpotLight :
    ([< | fespotlight_attr], [< | fespotlight_content], [> | fespotlight])
      star

  val feBlend :
    ([< | feblend_attr], [< | feblend_content], [> | feblend]) star

  val feColorMatrix :
    ([< | fecolormatrix_attr], [< | fecolormatrix_content],
     [> | fecolormatrix]) star

  val feComponentTransfer :
    ([< | fecomponenttransfer_attr], [< | fecomponenttransfer_content],
     [> | fecomponenttransfer]) star

  val feFuncA :
    ([< | fefunca_attr], [< | fefunca_content], [> | fefunca]) star

  val feFuncG :
    ([< | fefuncg_attr], [< | fefuncg_content], [> | fefuncg]) star

  val feFuncB :
    ([< | fefuncb_attr], [< | fefuncb_content], [> | fefuncb]) star

  val feFuncR :
    ([< | fefuncr_attr], [< | fefuncr_content], [> | fefuncr]) star

  val feComposite :
    ([< | fecomposite_attr], [< | fecomposite_content], [> | fecomposite])
      star

  val feConvolveMatrix :
    ([< | feconvolvematrix_attr], [< | feconvolvematrix_content],
     [> | feconvolvematrix]) star

  val feDiffuseLighting :
    ([< | fediffuselighting_attr], [< | fediffuselighting_content],
     [> | fediffuselighting]) star

  val feDisplacementMap :
    ([< | fedisplacementmap_attr], [< | fedisplacementmap_content],
     [> | fedisplacementmap]) star

  val feDropShadow :
    ([< | fedropshadow_attr], [< | fedropshadow_content],
     [> | fedropshadow]) star

  val feFlood :
    ([< | feflood_attr], [< | feflood_content], [> | feflood]) star

  val feGaussianBlur :
    ([< | fegaussianblur_attr], [< | fegaussianblur_content],
     [> | fegaussianblur]) star

  val feImage :
    ([< | feimage_attr], [< | feimage_content], [> | feimage]) star

  val feMerge :
    ([< | femerge_attr], [< | femerge_content], [> | femerge]) star

  val feMergeNode :
    ([< | femergenode_attr], [< | femergenode_content], [> | femergenode])
      star

  val feMorphology :
    ([< | femorphology_attr], [< | femorphology_content], [> | femorphology
                                                          ]) star

  val feOffset :
    ([< | feoffset_attr], [< | feoffset_content], [> | feoffset]) star

  val feSpecularLighting :
    ([< | fespecularlighting_attr], [< | fespecularlighting_content],
     [> | fespecularlighting]) star

  val feTile : ([< | fetile_attr], [< | fetile_content], [> | fetile]) star

  val feTurbulence :
    ([< | feturbulence_attr], [< | feturbulence_content], [> | feturbulence
                                                          ]) star

  val cursor : ([< | cursor_attr], [< | cursor_content], [> | cursor]) star

  val a : ([< | a_attr], [< | a_content], [> | a]) star

  val view : ([< | view_attr], [< | view_content], [> | view]) star

  val script :
    ([< | script_attr], [< | script_content], [> | script]) unary

  val animate :
    ([< | animate_attr], [< | animate_content], [> | animate]) star

  val set : ([< | set_attr], [< | set_content], [> | set]) star

  val animateMotion :
    ([< | animatemotion_attr], [< | animatemotion_content],
     [> | animatemotion]) star

  val mpath : ([< | mpath_attr], [< | mpath_content], [> | mpath]) star

  val animateColor :
    ([< | animatecolor_attr], [< | animatecolor_content], [> | animatecolor
                                                          ]) star

  val animateTransform :
    ([< | animatetransform_attr], [< | animatetransform_content],
     [> | animatetransform]) star

  val font : ([< | font_attr], [< | font_content], [> | font]) star
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val glyph : ([< | glyph_attr], [< | glyph_content], [> | glyph]) star
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val missing_glyph :
    ([< | missingglyph_attr], [< | missingglyph_content], [> | missingglyph
                                                          ]) star
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val hkern : ([< | hkern_attr], [> | hkern]) nullary
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val vkern : ([< | vkern_attr], [> | vkern]) nullary
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val font_face : ([< | font_face_attr], [> | font_face]) nullary
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val font_face_src :
    ([< | font_face_src_attr], [< | font_face_src_content], [> | font_face_src])
      star
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val font_face_uri :
    ([< | font_face_uri_attr], [< | font_face_uri_content], [> | font_face_uri])
      star
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val font_face_format :
    ([< | font_face_format_attr], [> | font_face_format]) nullary
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val font_face_name : ([< | font_face_name_attr], [> | font_face_name]) nullary
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val metadata :
    ?a: ((metadata_attr attrib) list) -> Xml.elt list_wrap -> [> | metadata] elt

  val foreignObject :
    ?a: ((foreignobject_attr attrib) list) ->
    Xml.elt list_wrap -> [> | foreignobject] elt

  (** {3 Deprecated} *)

  val pcdata : string wrap -> [> txt] elt
  [@@ocaml.deprecated "Use txt instead"]
  (** @deprecated Use txt instead *)

  val animation :
    ([< | animate_attr], [< | animate_content], [> | animate]) star
  [@@ocaml.deprecated "Use animate instead"]
  (** @deprecated Use animate instead *)

  (** {2 Conversion with untyped representation}

      WARNING: These functions do not ensure HTML or SVG validity! You should
      always explicitly given an appropriate type to the output.
  *)

  (** [import signal] converts the given XML signal into Tyxml elements.
      It can be used with HTML and SVG parsing libraries, such as Markup.
      @raise Xml_stream.Malformed_stream if the stream is malformed.
  *)
  val of_seq : Xml_stream.signal Seq.t -> 'a elt list_wrap

  val tot : Xml.elt -> 'a elt
  val totl : Xml.elt list_wrap -> ('a elt) list_wrap
  val toelt : 'a elt -> Xml.elt
  val toeltl : ('a elt) list_wrap -> Xml.elt list_wrap

  val doc_toelt : doc -> Xml.elt
  val to_xmlattribs : ('a attrib) list -> Xml.attrib list
  val to_attrib : Xml.attrib -> 'a attrib

  (** Unsafe features.

      Using this module can break
      SVG validity and may introduce security problems like
      code injection.
      Use it with care.
  *)
  module Unsafe : sig

    (** Insert raw text without any encoding *)
    val data : string wrap -> 'a elt

    (** Insert an XML node that is not implemented in this module.
        If it is a standard SVG node which is missing,
        please report to the Ocsigen team.
    *)
    val node : string -> ?a:'a attrib list -> 'b elt list_wrap -> 'c elt

    (** Insert an XML node without children
        that is not implemented in this module.
        If it is a standard SVG node which is missing,
        please report to the Ocsigen team.
    *)
    val leaf : string -> ?a:'a attrib list -> unit -> 'b elt

    (** Remove phantom type annotation on an element,
        to make it usable everywhere.
    *)
    val coerce_elt : 'a elt -> 'b elt

    (** Insert an attribute that is not implemented in this module.
        If it is a standard SVG attribute which is missing,
        please report to the Ocsigen team.
    *)
    val string_attrib : string -> string wrap -> 'a attrib

    (** Same, for float attribute *)
    val float_attrib : string -> float wrap -> 'a attrib

    (** Same, for int attribute *)
    val int_attrib : string -> int wrap -> 'a attrib

    (** Same, for URI attribute *)
    val uri_attrib : string -> uri wrap -> 'a attrib

    (** Same, for a space-separated list of values *)
    val space_sep_attrib : string -> string list wrap -> 'a attrib

    (** Same, for a comma-separated list of values *)
    val comma_sep_attrib : string -> string list wrap -> 'a attrib

  end

end

(** Equivalent to {!T}, but without wrapping. *)
module type NoWrap = T with module Xml.W = Xml_wrap.NoWrap


(** {2 Signature functors}
    See {{!page-"functors"}the manual of the functorial interface}. *)

(** Signature functor for {!Svg_f.Make}. *)
module Make (Xml : Xml_sigs.T) : sig

  (** See {!module-type:Svg_sigs.T}. *)
  module type T = T
    with type 'a Xml.W.t = 'a Xml.W.t
     and type 'a Xml.W.tlist = 'a Xml.W.tlist
     and type ('a,'b) Xml.W.ft = ('a,'b) Xml.W.ft
     and type Xml.uri = Xml.uri
     and type Xml.event_handler = Xml.event_handler
     and type Xml.mouse_event_handler = Xml.mouse_event_handler
     and type Xml.keyboard_event_handler = Xml.keyboard_event_handler
     and type Xml.touch_event_handler = Xml.touch_event_handler
     and type Xml.attrib = Xml.attrib
     and type Xml.elt = Xml.elt

end

(** Wrapped functions, to be used with {!Svg_f.Make_with_wrapped_functions}. *)
module type Wrapped_functions = sig

  module Xml : Xml_sigs.T

  val string_of_alignment_baseline :
    ([< Svg_types.alignment_baseline], string) Xml.W.ft

  val string_of_bool : (bool, string) Xml.W.ft

  val unoption_string : (string option, string) Xml.W.ft

  val string_of_big_variant : ([< Svg_types.big_variant], string) Xml.W.ft

  val string_of_coords : (Svg_types.coords, string) Xml.W.ft

  val string_of_dominant_baseline :
    ([< Svg_types.dominant_baseline], string) Xml.W.ft

  val string_of_fourfloats : (float * float * float * float, string) Xml.W.ft

  val string_of_in_value : ([< Svg_types.in_value], string) Xml.W.ft

  val string_of_int : (int, string) Xml.W.ft

  val string_of_length : (Svg_types.Unit.length, string) Xml.W.ft

  val string_of_lengths : (Svg_types.lengths, string) Xml.W.ft

  val string_of_number : (float, string) Xml.W.ft

  val string_of_number_optional_number :
    (float * float option, string) Xml.W.ft

  val string_of_numbers : (float list, string) Xml.W.ft

  val string_of_numbers_semicolon : (float list, string) Xml.W.ft

  val string_of_offset : ([< Svg_types.offset], string) Xml.W.ft

  val string_of_orient : (Svg_types.Unit.angle option, string) Xml.W.ft

  val string_of_paint : ([< Svg_types.paint], string) Xml.W.ft
  
  val string_of_fill_rule : ([< Svg_types.fill_rule], string) Xml.W.ft

  val string_of_strokedasharray : (Svg_types.lengths, string) Xml.W.ft

  val string_of_transform : (Svg_types.transform, string) Xml.W.ft

  val string_of_transforms : (Svg_types.transforms, string) Xml.W.ft

end
