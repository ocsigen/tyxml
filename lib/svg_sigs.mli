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
  type +'a data

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
  type 'a elt = 'a data Xml.Elt.t

  (** A complete SVG document. *)
  type doc = [ `Svg ] elt
  
  (** [list_wrap] is a containre for list of elements.

      In most cases, ['a list_wrap = 'a list]. For [R] modules (in eliom or js_of_ocaml),
      It will be {!ReactiveData.RList.t}.
  *)
  type 'a child = 'a data Xml.Child.t
  type 'a children = 'a data Xml.Child.list

  type 'a attr_wrap = 'a Xml.Attr.t

  (** A nullary element is an element that doesn't have any children. *)
  type ('a, 'b) nullary = ?a: (('a attrib) list) -> unit -> 'b elt

  (** A unary element is an element that have exactly one children. *)
  type ('a, 'b, 'c) unary = ?a: (('a attrib) list) -> 'b child -> 'c elt

  (** A star element is an element that has any number of children, including zero. *)
  type ('a, 'b, 'c) star =
    ?a: (('a attrib) list) -> 'b children -> 'c elt

  (** Various information about SVG, such as the doctype, ... *)
  module Info : Xml_sigs.Info

  (** {3 Uri} *)

  type uri = Xml.uri
  val string_of_uri : (uri, string) Xml.Attr.ft
  val uri_of_string : (string, uri) Xml.Attr.ft

  open Svg_types

  (** {2:attributes Attributes } *)

  val a_version : string attr_wrap -> [> | `Version ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_baseProfile : string attr_wrap -> [> | `BaseProfile ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_x : coord attr_wrap -> [> | `X ] attrib

  val a_y : coord attr_wrap -> [> | `Y ] attrib

  val a_width : Unit.length attr_wrap -> [> | `Width ] attrib

  val a_height : Unit.length attr_wrap -> [> | `Height ] attrib

  val a_preserveAspectRatio : string attr_wrap -> [> | `PreserveAspectRatio ] attrib

  val a_contentScriptType : string attr_wrap -> [> | `ContentScriptType ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_contentStyleType : string attr_wrap -> [> | `ContentStyleType ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_zoomAndPan : [< | `Disable | `Magnify ] attr_wrap -> [> | `ZoomAndSpan ] attrib

  val a_href : iri attr_wrap -> [> | `Xlink_href ] attrib

  val a_xlink_href : iri attr_wrap -> [> | `Xlink_href ] attrib
    [@@ocaml.deprecated "Use a_href"]
  (** @deprecated Use a_href *)

  val a_requiredFeatures : spacestrings attr_wrap -> [> | `RequiredFeatures ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_requiredExtensions :
    spacestrings attr_wrap -> [> | `RequiredExtension ] attrib

  val a_systemLanguage : commastrings attr_wrap -> [> | `SystemLanguage ] attrib

  val a_externalRessourcesRequired :
    bool attr_wrap -> [> | `ExternalRessourcesRequired ] attrib

  val a_id : string attr_wrap -> [> | `Id ] attrib

  val a_user_data : string -> string attr_wrap -> [> | `User_data] attrib

  val a_xml_base : iri attr_wrap -> [> | `Xml_Base ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_xml_lang : iri attr_wrap -> [> | `Xml_Lang ] attrib

  val a_xml_space : [< `Default | `Preserve ] attr_wrap -> [> | `Xml_Space ] attrib
    [@@ocaml.deprecated "Use CSS white-space"]
  (** @deprecated Use CSS white-space *)

  val a_type : string attr_wrap -> [> | `Type ] attrib

  val a_media : commastrings attr_wrap -> [> | `Media ] attrib

  val a_xlink_title : string attr_wrap -> [> | `Title ] attrib
    [@@ocaml.deprecated "Use a child title element"]
  (** @deprecated Use a child title element *)

  val a_class : spacestrings attr_wrap -> [> | `Class ] attrib

  val a_style : string attr_wrap -> [> | `Style ] attrib

  val a_transform : transforms attr_wrap -> [> | `Transform ] attrib

  val a_viewBox : fourfloats attr_wrap -> [> | `ViewBox ] attrib

  val a_d : string attr_wrap -> [> | `D ] attrib

  val a_pathLength : float attr_wrap -> [> | `PathLength ] attrib

  (* XXX: better language support *)
  val a_rx : Unit.length attr_wrap -> [> | `Rx ] attrib

  val a_ry : Unit.length attr_wrap -> [> | `Ry ] attrib

  val a_cx : Unit.length attr_wrap -> [> | `Cx ] attrib

  val a_cy : Unit.length attr_wrap -> [> | `Cy ] attrib

  val a_r : Unit.length attr_wrap -> [> | `R ] attrib

  val a_x1 : coord attr_wrap -> [> | `X1 ] attrib

  val a_y1 : coord attr_wrap -> [> | `Y1 ] attrib

  val a_x2 : coord attr_wrap -> [> | `X2 ] attrib

  val a_y2 : coord attr_wrap -> [> | `Y2 ] attrib

  val a_points : coords attr_wrap -> [> | `Points ] attrib

  val a_x_list : lengths attr_wrap -> [> | `X_list ] attrib
    [@@reflect.attribute "x" ["text"; "tspan"; "tref"; "altGlyph"]]

  val a_y_list : lengths attr_wrap -> [> | `Y_list ] attrib
    [@@reflect.attribute "y" ["text"; "tspan"; "tref"; "altGlyph"]]

  val a_dx : number attr_wrap -> [> | `Dx ] attrib

  val a_dy : number attr_wrap -> [> | `Dy ] attrib

  val a_dx_list : lengths attr_wrap -> [> | `Dx_list ] attrib
    [@@reflect.attribute "dx" ["text"; "tspan"; "tref"; "altGlyph"]]

  val a_dy_list : lengths attr_wrap -> [> | `Dy_list ] attrib
    [@@reflect.attribute "dy" ["text"; "tspan"; "tref"; "altGlyph"]]

  val a_lengthAdjust :
    [< `Spacing | `SpacingAndGlyphs ] attr_wrap -> [> | `LengthAdjust ] attrib

  val a_textLength : Unit.length attr_wrap -> [> | `TextLength ] attrib

  val a_text_anchor : [< `Start | `Middle | `End | `Inherit ] attr_wrap -> [> | `Text_Anchor ] attrib

  val a_text_decoration : [< `None | `Underline | `Overline | `Line_through | `Blink | `Inherit ] attr_wrap -> [> | `Text_Decoration ] attrib

  val a_text_rendering : [< `Auto | `OptimizeSpeed | `OptimizeLegibility | `GeometricPrecision | `Inherit ] attr_wrap -> [> | `Text_Rendering ] attrib

  val a_rotate : numbers attr_wrap -> [> | `Rotate ] attrib

  val a_startOffset : Unit.length attr_wrap -> [> | `StartOffset ] attrib

  val a_method : [< `Align | `Stretch ] attr_wrap -> [> | `Method ] attrib

  val a_spacing : [< `Auto | `Exact ] attr_wrap -> [> | `Spacing ] attrib

  val a_glyphRef : string attr_wrap -> [> | `GlyphRef ] attrib

  val a_format : string attr_wrap -> [> | `Format ] attrib

  val a_markerUnits :
    [< `StrokeWidth | `UserSpaceOnUse ] attr_wrap -> [> | `MarkerUnits ] attrib

  val a_refX : coord attr_wrap -> [> | `RefX ] attrib

  val a_refY : coord attr_wrap -> [> | `RefY ] attrib

  val a_markerWidth : Unit.length attr_wrap -> [> | `MarkerWidth ] attrib

  val a_markerHeight : Unit.length attr_wrap -> [> | `MarkerHeight ] attrib

  val a_orient : Unit.angle option attr_wrap -> [> | `Orient ] attrib

  val a_local : string attr_wrap -> [> | `Local ] attrib

  val a_rendering_intent :
    [<
      | `Auto
      | `Perceptual
      | `Relative_colorimetric
      | `Saturation
      | `Absolute_colorimetric ] attr_wrap -> [> | `Rendering_Indent ] attrib

  val a_gradientUnits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] attr_wrap ->
    [ | `GradientUnits ] attrib

  val a_gradientTransform : transforms attr_wrap -> [> | `Gradient_Transform ] attrib

  val a_spreadMethod :
    [< `Pad | `Reflect | `Repeat ] attr_wrap -> [> | `SpreadMethod ] attrib

  val a_fx : coord attr_wrap -> [> | `Fx ] attrib

  val a_fy : coord attr_wrap -> [> | `Fy ] attrib

  val a_offset :
    [< `Number of number | `Percentage of percentage ] attr_wrap ->
    [> | `Offset ] attrib

  val a_patternUnits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] attr_wrap ->
    [> | `PatternUnits ] attrib

  val a_patternContentUnits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] attr_wrap ->
    [> | `PatternContentUnits ] attrib

  val a_patternTransform : transforms attr_wrap -> [> | `PatternTransform ] attrib

  val a_clipPathUnits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] attr_wrap ->
    [> | `ClipPathUnits ] attrib

  val a_maskUnits :
    [< | `UserSpaceOnUse | `ObjectBoundingBox ] attr_wrap -> [> | `MaskUnits ] attrib

  val a_maskContentUnits :
    [< | `UserSpaceOnUse | `ObjectBoundingBox ] attr_wrap ->
    [> | `MaskContentUnits ] attrib

  val a_primitiveUnits :
    [< | `UserSpaceOnUse | `ObjectBoundingBox ] attr_wrap ->
    [> | `PrimitiveUnits ] attrib

  val a_filterRes : number_optional_number attr_wrap -> [> | `FilterResUnits ] attrib

  val a_result : string attr_wrap -> [> | `Result ] attrib

  val a_in :
    [<
      | `SourceGraphic
      | `SourceAlpha
      | `BackgroundImage
      | `BackgroundAlpha
      | `FillPaint
      | `StrokePaint
      | `Ref of string ] attr_wrap -> [> | `In ] attrib

  val a_in2 :
    [<
      | `SourceGraphic
      | `SourceAlpha
      | `BackgroundImage
      | `BackgroundAlpha
      | `FillPaint
      | `StrokePaint
      | `Ref of string ] attr_wrap -> [> | `In2 ] attrib

  val a_azimuth : float attr_wrap -> [> | `Azimuth ] attrib

  val a_elevation : float attr_wrap -> [> | `Elevation ] attrib

  val a_pointsAtX : float attr_wrap -> [> | `PointsAtX ] attrib

  val a_pointsAtY : float attr_wrap -> [> | `PointsAtY ] attrib

  val a_pointsAtZ : float attr_wrap -> [> | `PointsAtZ ] attrib

  val a_specularExponent : float attr_wrap -> [> | `SpecularExponent ] attrib

  val a_specularConstant : float attr_wrap -> [> | `SpecularConstant ] attrib

  val a_limitingConeAngle : float attr_wrap -> [> | `LimitingConeAngle ] attrib

  val a_mode :
    [< | `Normal | `Multiply | `Screen | `Darken | `Lighten ] attr_wrap ->
    [> | `Mode ] attrib

  val a_feColorMatrix_type :
    [< | `Matrix | `Saturate | `HueRotate | `LuminanceToAlpha ] attr_wrap ->
    [> | `Typefecolor ] attrib
    [@@reflect.attribute "type" ["feColorMatrix"]]

  val a_values : numbers attr_wrap -> [> | `Values ] attrib

  val a_transfer_type :
    [< | `Identity | `Table | `Discrete | `Linear | `Gamma ] attr_wrap ->
    [> | `Type_transfert ] attrib
    [@@reflect.attribute "type" ["feFuncR"; "feFuncG"; "feFuncB"; "feFuncA"]]

  val a_tableValues : numbers attr_wrap -> [> | `TableValues ] attrib

  val a_intercept : number attr_wrap -> [> | `Intercept ] attrib

  val a_amplitude : number attr_wrap -> [> | `Amplitude ] attrib

  val a_exponent : number attr_wrap -> [> | `Exponent ] attrib

  val a_transfer_offset : number attr_wrap -> [> | `Offset_transfer ] attrib
    [@@reflect.attribute "offset" ["feFuncR"; "feFuncG"; "feFuncB"; "feFuncA"]]

  val a_feComposite_operator :
    [< | `Over | `In | `Out | `Atop | `Xor | `Arithmetic ] attr_wrap ->
    [> | `OperatorComposite ] attrib
    [@@reflect.attribute "operator" ["feComposite"]]

  val a_k1 : number attr_wrap -> [> | `K1 ] attrib

  val a_k2 : number attr_wrap -> [> | `K2 ] attrib

  val a_k3 : number attr_wrap -> [> | `K3 ] attrib

  val a_k4 : number attr_wrap -> [> | `K4 ] attrib

  val a_order : number_optional_number attr_wrap -> [> | `Order ] attrib

  val a_kernelMatrix : numbers attr_wrap -> [> | `KernelMatrix ] attrib

  val a_divisor : number attr_wrap -> [> | `Divisor ] attrib

  val a_bias : number attr_wrap -> [> | `Bias ] attrib

  val a_kernelUnitLength :
    number_optional_number attr_wrap -> [> | `KernelUnitLength ] attrib

  val a_targetX : int attr_wrap -> [> | `TargetX ] attrib

  val a_targetY : int attr_wrap -> [> | `TargetY ] attrib

  val a_edgeMode :
    [< | `Duplicate | `Wrap | `None ] attr_wrap -> [> | `TargetY ] attrib

  val a_preserveAlpha : bool attr_wrap -> [> | `TargetY ] attrib

  val a_surfaceScale : number attr_wrap -> [> | `SurfaceScale ] attrib

  val a_diffuseConstant : number attr_wrap -> [> | `DiffuseConstant ] attrib

  val a_scale : number attr_wrap -> [> | `Scale ] attrib

  val a_xChannelSelector :
    [< | `R | `G | `B | `A ] attr_wrap -> [> | `XChannelSelector ] attrib

  val a_yChannelSelector :
    [< | `R | `G | `B | `A ] attr_wrap -> [> | `YChannelSelector ] attrib

  val a_stdDeviation : number_optional_number attr_wrap -> [> | `StdDeviation ] attrib

  val a_feMorphology_operator :
    [< | `Erode | `Dilate ] attr_wrap -> [> | `OperatorMorphology ] attrib
    [@@reflect.attribute "operator" ["feMorphology"]]

  val a_radius : number_optional_number attr_wrap -> [> | `Radius ] attrib

  val a_baseFrenquency :
    number_optional_number attr_wrap -> [> | `BaseFrequency ] attrib

  val a_numOctaves : int attr_wrap -> [> | `NumOctaves ] attrib

  val a_seed : number attr_wrap -> [> | `Seed ] attrib

  val a_stitchTiles :
    [< | `Stitch | `NoStitch ] attr_wrap -> [> | `StitchTiles ] attrib

  val a_feTurbulence_type :
    [< | `FractalNoise | `Turbulence ] attr_wrap -> [> | `TypeStitch ] attrib
    [@@reflect.attribute "type" ["feTurbulence"]]

  val a_xlink_show : [< | `New | `Replace ] attr_wrap -> [> | `Xlink_show ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_xlink_actuate :
    [< | `OnRequest | `OnLoad | `Other | `None ] attr_wrap
    -> [> | `Xlink_actuate ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_target : string attr_wrap -> [> | `Xlink_target ] attrib

  val a_viewTarget : string attr_wrap -> [> | `ViewTarget ] attrib
    [@@ocaml.deprecated "Removed in SVG2"]
  (** @deprecated Removed in SVG2 *)

  val a_attributeName : string attr_wrap -> [> | `AttributeName ] attrib

  val a_attributeType :
    [< | `CSS | `XML | `Auto ] attr_wrap -> [> | `AttributeType ] attrib

  val a_begin : string attr_wrap -> [> | `Begin ] attrib

  val a_dur : string attr_wrap -> [> | `Dur ] attrib

  val a_min : string attr_wrap -> [> | `Min ] attrib

  val a_max : string attr_wrap -> [> | `Max ] attrib

  val a_restart :
    [< | `Always | `WhenNotActive | `Never ] attr_wrap -> [> | `Restart ] attrib

  val a_repeatCount : string attr_wrap -> [> | `RepeatCount ] attrib

  val a_repeatDur : string attr_wrap -> [> | `RepeatDur ] attrib

  val a_fill : paint attr_wrap -> [> | `Fill ] attrib

  val a_animation_fill : [< | `Freeze | `Remove ] attr_wrap -> [> | `Fill_Animation ] attrib
    [@@reflect.attribute "fill" ["animation"]]

  val a_calcMode :
    [< | `Discrete | `Linear | `Paced | `Spline ] attr_wrap -> [> | `CalcMode ] attrib

  val a_animation_values : strings attr_wrap -> [> | `Valuesanim ] attrib
    [@@reflect.attribute "values" ["animation"]]

  val a_keyTimes : strings attr_wrap -> [> | `KeyTimes ] attrib

  val a_keySplines : strings attr_wrap -> [> | `KeySplines ] attrib

  val a_from : string attr_wrap -> [> | `From ] attrib

  val a_to : string attr_wrap -> [> | `To ] attrib

  val a_by : string attr_wrap -> [> | `By ] attrib

  val a_additive : [< | `Replace | `Sum ] attr_wrap -> [> | `Additive ] attrib

  val a_accumulate : [< | `None | `Sum ] attr_wrap -> [> | `Accumulate ] attrib

  val a_keyPoints : numbers_semicolon attr_wrap -> [> | `KeyPoints ] attrib

  val a_path : string attr_wrap -> [> | `Path ] attrib

  val a_animateTransform_type :
    [ | `Translate | `Scale | `Rotate | `SkewX | `SkewY ] attr_wrap ->
    [ | `Typeanimatetransform ] attrib
    [@@reflect.attribute "type" ["animateTransform"]]

  val a_horiz_origin_x : number attr_wrap -> [> | `HorizOriginX ] attrib

  val a_horiz_origin_y : number attr_wrap -> [> | `HorizOriginY ] attrib

  val a_horiz_adv_x : number attr_wrap -> [> | `HorizAdvX ] attrib

  val a_vert_origin_x : number attr_wrap -> [> | `VertOriginX ] attrib

  val a_vert_origin_y : number attr_wrap -> [> | `VertOriginY ] attrib

  val a_vert_adv_y : number attr_wrap -> [> | `VertAdvY ] attrib

  val a_unicode : string attr_wrap -> [> | `Unicode ] attrib

  val a_glyph_name : string attr_wrap -> [> | `glyphname ] attrib

  val a_orientation : [< | `H | `V ] attr_wrap -> [> | `Orientation ] attrib

  val a_arabic_form :
    [< | `Initial | `Medial | `Terminal | `Isolated ] attr_wrap ->
    [> | `Arabicform ] attrib

  val a_lang : string attr_wrap -> [> | `Lang ] attrib

  val a_u1 : string attr_wrap -> [> | `U1 ] attrib

  val a_u2 : string attr_wrap -> [> | `U2 ] attrib

  val a_g1 : string attr_wrap -> [> | `G1 ] attrib

  val a_g2 : string attr_wrap -> [> | `G2 ] attrib

  val a_k : string attr_wrap -> [> | `K ] attrib

  val a_font_family : string attr_wrap -> [> | `Font_Family ] attrib

  val a_font_style : string attr_wrap -> [> | `Font_Style ] attrib

  val a_font_variant : string attr_wrap -> [> | `Font_Variant ] attrib

  val a_font_weight : string attr_wrap -> [> | `Font_Weight ] attrib

  val a_font_stretch : string attr_wrap -> [> | `Font_Stretch ] attrib

  val a_font_size : string attr_wrap -> [> | `Font_Size ] attrib

  val a_unicode_range : string attr_wrap -> [> | `UnicodeRange ] attrib

  val a_units_per_em : string attr_wrap -> [> | `UnitsPerEm ] attrib

  val a_stemv : number attr_wrap -> [> | `Stemv ] attrib

  val a_stemh : number attr_wrap -> [> | `Stemh ] attrib

  val a_slope : number attr_wrap -> [> | `Slope ] attrib

  val a_cap_height : number attr_wrap -> [> | `CapHeight ] attrib

  val a_x_height : number attr_wrap -> [> | `XHeight ] attrib

  val a_accent_height : number attr_wrap -> [> | `AccentHeight ] attrib

  val a_ascent : number attr_wrap -> [> | `Ascent ] attrib

  val a_widths : string attr_wrap -> [> | `Widths ] attrib

  val a_bbox : string attr_wrap -> [> | `Bbox ] attrib

  val a_ideographic : number attr_wrap -> [> | `Ideographic ] attrib

  val a_alphabetic : number attr_wrap -> [> | `Alphabetic ] attrib

  val a_mathematical : number attr_wrap -> [> | `Mathematical ] attrib

  val a_hanging : number attr_wrap -> [> | `Hanging ] attrib

  val a_videographic : number attr_wrap -> [> | `VIdeographic ] attrib

  val a_v_alphabetic : number attr_wrap -> [> | `VAlphabetic ] attrib

  val a_v_mathematical : number attr_wrap -> [> | `VMathematical ] attrib

  val a_v_hanging : number attr_wrap -> [> | `VHanging ] attrib

  val a_underline_position : number attr_wrap -> [> | `UnderlinePosition ] attrib

  val a_underline_thickness : number attr_wrap -> [> | `UnderlineThickness ] attrib

  val a_strikethrough_position :
    number attr_wrap -> [> | `StrikethroughPosition ] attrib

  val a_strikethrough_thickness :
    number attr_wrap -> [> | `StrikethroughThickness ] attrib

  val a_overline_position : number attr_wrap -> [> | `OverlinePosition ] attrib

  val a_overline_thickness : number attr_wrap -> [> | `OverlineThickness ] attrib

  val a_string : string attr_wrap -> [> | `String ] attrib

  val a_name : string attr_wrap -> [> | `Name ] attrib

  val a_alignment_baseline :
    [< | `Auto | `Baseline | `Before_edge | `Text_before_edge | `Middle
       | `Central | `After_edge | `Text_after_edge | `Ideographic
       | `Alphabetic | `Hanging | `Mathematical | `Inherit ] attr_wrap ->
    [> | `Alignment_Baseline ] attrib

  val a_dominant_baseline :
    [< | `Auto | `Use_script | `No_change | `Reset_size | `Ideographic
       | `Alphabetic | `Hanging | `Mathematical | `Central | `Middle
       | `Text_after_edge | `Text_before_edge | `Inherit ] attr_wrap ->
    [> | `Dominant_Baseline ] attrib

  val a_stop_color : color attr_wrap -> [> | `Stop_Color ] attrib

  val a_stop_opacity : number attr_wrap -> [> | `Stop_Opacity ] attrib

  val a_stroke : paint attr_wrap -> [> | `Stroke ] attrib

  val a_stroke_width : Unit.length attr_wrap -> [> | `Stroke_Width ] attrib

  val a_stroke_linecap :
    [< `Butt | `Round | `Square ] attr_wrap -> [> | `Stroke_Linecap ] attrib

  val a_stroke_linejoin :
    [< `Miter | `Round | `Bever ] attr_wrap -> [> `Stroke_Linejoin ] attrib

  val a_stroke_miterlimit : float attr_wrap -> [> `Stroke_Miterlimit ] attrib

  val a_stroke_dasharray :
    Unit.length list attr_wrap -> [> `Stroke_Dasharray ] attrib

  val a_stroke_dashoffset : Unit.length attr_wrap -> [> `Stroke_Dashoffset ] attrib

  val a_stroke_opacity : float attr_wrap -> [> `Stroke_Opacity ] attrib

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

  val txt : string Xml.Elt.child -> [> | txt] elt

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

  val feFlood :
    ([< | feflood_attr], [< | feflood_content], [> | feflood]) star

  val feGaussianBlur :
    ([< | fegaussianblur_attr], [< | fegaussianblur_content],
     [> | fegaussianblur]) star

  val feImage :
    ([< | feimage_attr], [< | feimage_content], [> | feimage]) star

  val feMerge :
    ([< | femerge_attr], [< | femerge_content], [> | femerge]) star

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

  val animation :
    ([< | animation_attr], [< | animation_content], [> | animation]) star

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
    ?a: ((metadata_attr attrib) list) -> Xml.children -> [> | metadata] elt

  val foreignObject :
    ?a: ((foreignobject_attr attrib) list) ->
    Xml.children -> [> | foreignobject] elt

  (** {3 Deprecated} *)

  val pcdata : string Xml.Child.t -> [> txt] elt
  [@@ocaml.deprecated "Use txt instead"]
  (** @deprecated Use txt instead *)

  (** {2 Conversion with untyped representation} 

      WARNING: These functions do not ensure HTML or SVG validity! You should
      always explicitly given an appropriate type to the output.
  *)

  (** [import signal] converts the given XML signal into Tyxml elements.
      It can be used with HTML and SVG parsing libraries, such as Markup.
      @raise Xml_stream.Malformed_stream if the stream is malformed.
  *)
  val of_seq : Xml_stream.signal Seq.t -> 'a children

  val tot : Xml.elt -> 'a elt
  val totl : Xml.children -> 'a children
  val toelt : 'a elt -> Xml.elt
  val toeltl : 'a children -> Xml.children

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
    val data : string Xml.Child.t -> 'a elt

    (** Insert an XML node that is not implemented in this module.
        If it is a standard SVG node which is missing,
        please report to the Ocsigen team.
    *)
    val node : string -> ?a:'a attrib list -> 'b children -> 'c elt

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
    val string_attrib : string -> string attr_wrap -> 'a attrib

    (** Same, for float attribute *)
    val float_attrib : string -> float attr_wrap -> 'a attrib

    (** Same, for int attribute *)
    val int_attrib : string -> int attr_wrap -> 'a attrib

    (** Same, for URI attribute *)
    val uri_attrib : string -> uri attr_wrap -> 'a attrib

    (** Same, for a space separated list of values *)
    val space_sep_attrib : string -> string list attr_wrap -> 'a attrib

    (** Same, for a comma separated list of values *)
    val comma_sep_attrib : string -> string list attr_wrap -> 'a attrib

  end

end

(** Equivalent to {!T}, but without wrapping. *)
module type NoWrap = T
  with module Xml.Elt = Xml_wrap.NoWrap
   and module Xml.Attr = Xml_wrap.NoWrap


(** {2 Signature functors}
    See {% <<a_manual chapter="functors"|the manual of the functorial interface>> %}. *)

(** Signature functor for {!Svg_f.Make}. *)
module Make (Xml : Xml_sigs.T) : sig

  (** See {!module-type:Svg_sigs.T}. *)
  module type T = T
    with type 'a Xml.Elt.t = 'a Xml.Elt.t
     and type 'a Xml.Elt.child = 'a Xml.Elt.child
     and type 'a Xml.Child.list = 'a Xml.Child.list
     and type 'a Xml.Attr.t = 'a Xml.Attr.t
     and type ('a,'b) Xml.Attr.ft = ('a,'b) Xml.Attr.ft
     and type Xml.uri = Xml.uri
     and type Xml.event_handler = Xml.event_handler
     and type Xml.mouse_event_handler = Xml.mouse_event_handler
     and type Xml.keyboard_event_handler = Xml.keyboard_event_handler
     and type Xml.touch_event_handler = Xml.touch_event_handler
     and type Xml.attrib = Xml.attrib
     and type Xml.data = Xml.data

end

(** Wrapped functions, to be used with {!Svg_f.Make_with_wrapped_functions}. *)
module type Wrapped_functions = sig

  module Xml : Xml_sigs.T

  val string_of_alignment_baseline :
    ([< Svg_types.alignment_baseline], string) Xml.Attr.ft

  val string_of_bool : (bool, string) Xml.Attr.ft

  val string_of_big_variant : ([< Svg_types.big_variant], string) Xml.Attr.ft

  val string_of_coords : (Svg_types.coords, string) Xml.Attr.ft

  val string_of_dominant_baseline :
    ([< Svg_types.dominant_baseline], string) Xml.Attr.ft

  val string_of_fourfloats : (float * float * float * float, string) Xml.Attr.ft

  val string_of_in_value : ([< Svg_types.in_value], string) Xml.Attr.ft

  val string_of_int : (int, string) Xml.Attr.ft

  val string_of_length : (Svg_types.Unit.length, string) Xml.Attr.ft

  val string_of_lengths : (Svg_types.lengths, string) Xml.Attr.ft

  val string_of_number : (float, string) Xml.Attr.ft

  val string_of_number_optional_number :
    (float * float option, string) Xml.Attr.ft

  val string_of_numbers : (float list, string) Xml.Attr.ft

  val string_of_numbers_semicolon : (float list, string) Xml.Attr.ft

  val string_of_offset : ([< Svg_types.offset], string) Xml.Attr.ft

  val string_of_orient : (Svg_types.Unit.angle option, string) Xml.Attr.ft

  val string_of_paint : ([< Svg_types.paint], string) Xml.Attr.ft

  val string_of_strokedasharray : (Svg_types.lengths, string) Xml.Attr.ft

  val string_of_transform : (Svg_types.transform, string) Xml.Attr.ft

  val string_of_transforms : (Svg_types.transforms, string) Xml.Attr.ft

end
