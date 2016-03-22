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

  open Svg_types
  open Unit

  module Xml : Xml_sigs.T
  module Info : Xml_sigs.Info

  type uri = Xml.uri
  val string_of_uri : (uri, string) Xml.W.ft
  val uri_of_string : (string, uri) Xml.W.ft

  (** {1 Abstraction over XML's types} *)

  type +'a attrib

  type 'a wrap = 'a Xml.W.t
  type 'a list_wrap = 'a Xml.W.tlist

  type +'a elt

  type ('a, 'b) nullary = ?a: (('a attrib) list) -> unit -> 'b elt

  type ('a, 'b, 'c) unary = ?a: (('a attrib) list) -> 'b elt wrap -> 'c elt

  type ('a, 'b, 'c) star =
    ?a: (('a attrib) list) -> ('b elt) list_wrap -> 'c elt

  (* to be processed by a script *)
  type altglyphdef_content =
    [ | `Ref of (glyphref elt) list | `Item of (altglyphitem elt) list
    ]

  val pcdata : string wrap -> [> | `PCDATA] elt

  (** {1 attributes } *)

  val a_version : string wrap -> [> | `Version ] attrib

  val a_baseProfile : string wrap -> [> | `BaseProfile ] attrib

  val a_x : coord wrap -> [> | `X ] attrib

  val a_y : coord wrap -> [> | `Y ] attrib

  val a_width : Unit.length wrap -> [> | `Width ] attrib

  val a_height : Unit.length wrap -> [> | `Height ] attrib

  val a_preserveAspectRatio : string wrap -> [> | `PreserveAspectRatio ] attrib

  val a_contentScriptType : string wrap -> [> | `ContentScriptType ] attrib

  val a_contentStyleType : string wrap -> [> | `ContentStyleType ] attrib

  val a_zoomAndPan : [< | `Disable | `Magnify ] wrap -> [> | `ZoomAndSpan ] attrib

  val a_xlink_href : iri wrap -> [> | `Xlink_href ] attrib

  val a_requiredFeatures : spacestrings wrap -> [> | `RequiredFeatures ] attrib

  val a_requiredExtensions :
    spacestrings wrap -> [> | `RequiredExtension ] attrib

  val a_systemLanguage : commastrings wrap -> [> | `SystemLanguage ] attrib

  val a_externalRessourcesRequired :
    bool wrap -> [> | `ExternalRessourcesRequired ] attrib

  val a_id : string wrap -> [> | `Id ] attrib

  val a_xml_base : iri wrap -> [> | `Xml_Base ] attrib

  val a_xml_lang : iri wrap -> [> | `Xml_Lang ] attrib

  val a_xml_space : [< `Default | `Preserve ] wrap -> [> | `Xml_Space ] attrib

  val a_type : string wrap -> [> | `Type ] attrib

  val a_media : commastrings wrap -> [> | `Media ] attrib

  val a_title : string wrap -> [> | `Title ] attrib

  val a_class : spacestrings wrap -> [> | `Class ] attrib

  val a_style : string wrap -> [> | `Style ] attrib

  val a_transform : transform wrap -> [> | `Transform ] attrib

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

  val a_y_list : lengths wrap -> [> | `Y_list ] attrib

  val a_dx : number wrap -> [> | `Dx ] attrib

  val a_dy : number wrap -> [> | `Dy ] attrib

  val a_dx_list : lengths wrap -> [> | `Dx_list ] attrib

  val a_dy_list : lengths wrap -> [> | `Dy_list ] attrib

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
      | `Absolute_colorimetric ] wrap -> [> | `Rendering_Indent ] attrib

  val a_gradientUnits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
    [ | `GradientUnits ] attrib

  val a_gradientTransform : transforms wrap -> [> | `Gradient_Transform ] attrib

  val a_spreadMethod :
    [< `Pad | `Reflect | `Repeat ] wrap -> [> | `SpreadMethod ] attrib

  val a_fx : coord wrap -> [> | `Fx ] attrib

  val a_fy : coord wrap -> [> | `Fy ] attrib

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

  val a_clipPathUnits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
    [> | `ClipPathUnits ] attrib

  val a_maskUnits :
    [< | `UserSpaceOnUse | `ObjectBoundingBox ] wrap -> [> | `MaskUnits ] attrib

  val a_maskContentUnits :
    [< | `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
    [> | `MaskContentUnits ] attrib

  val a_primitiveUnits :
    [< | `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
    [> | `PrimitiveUnits ] attrib

  val a_filterRes : number_optional_number wrap -> [> | `FilterResUnits ] attrib

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

  val a_pointsAtX : float wrap -> [> | `PointsAtX ] attrib

  val a_pointsAtY : float wrap -> [> | `PointsAtY ] attrib

  val a_pointsAtZ : float wrap -> [> | `PointsAtZ ] attrib

  val a_specularExponent : float wrap -> [> | `SpecularExponent ] attrib

  val a_specularConstant : float wrap -> [> | `SpecularConstant ] attrib

  val a_limitingConeAngle : float wrap -> [> | `LimitingConeAngle ] attrib

  val a_mode :
    [< | `Normal | `Multiply | `Screen | `Darken | `Lighten ] wrap ->
    [> | `Mode ] attrib

  val a_feColorMatrix_type :
    [< | `Matrix | `Saturate | `HueRotate | `LuminanceToAlpha ] wrap ->
    [> | `Typefecolor ] attrib

  val a_values : numbers wrap -> [> | `Values ] attrib

  val a_type_transfer :
    [< | `Identity | `Table | `Discrete | `Linear | `Gamma ] wrap ->
    [> | `Type_transfert ] attrib

  val a_tableValues : numbers wrap -> [> | `TableValues ] attrib

  val a_intercept : number wrap -> [> | `Intercept ] attrib

  val a_amplitude : number wrap -> [> | `Amplitude ] attrib

  val a_exponent : number wrap -> [> | `Exponent ] attrib

  val a_offset_transfer : number wrap -> [> | `Offset_transfer ] attrib

  val a_operator :
    [< | `Over | `In | `Out | `Atop | `Xor | `Arithmetic ] wrap ->
    [> | `Operator ] attrib

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
    [< | `Duplicate | `Wrap | `None ] wrap -> [> | `TargetY ] attrib

  val a_preserveAlpha : bool wrap -> [> | `TargetY ] attrib

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

  val a_radius : number_optional_number wrap -> [> | `Radius ] attrib

  val a_baseFrenquency :
    number_optional_number wrap -> [> | `BaseFrequency ] attrib

  val a_numOctaves : int wrap -> [> | `NumOctaves ] attrib

  val a_seed : number wrap -> [> | `Seed ] attrib

  val a_stitchTiles :
    [< | `Stitch | `NoStitch ] wrap -> [> | `StitchTiles ] attrib

  val a_feTurbulence_type :
    [< | `FractalNoise | `Turbulence ] wrap -> [> | `TypeStitch ] attrib

  val a_xlink_show : [< | `New | `Replace ] wrap -> [> | `Xlink_show ] attrib

  val a_xlink_actuate :
    [< | `OnRequest | `OnLoad | `Other | `None ] wrap
    -> [> | `Xlink_actuate ] attrib

  val a_target : string wrap -> [> | `Xlink_target ] attrib

  val a_viewTarget : string wrap -> [> | `ViewTarget ] attrib

  val a_attributeName : string wrap -> [> | `AttributeName ] attrib

  val a_attributeType :
    [< | `CSS | `XML | `Auto ] wrap -> [> | `AttributeType ] attrib

  val a_begin : string wrap -> [> | `Begin ] attrib

  val a_dur : string wrap -> [> | `Dur ] attrib

  (* XXX*)
  val a_min : string wrap -> [> | `Min ] attrib

  (* XXX *)
  val a_max : string wrap -> [> | `Max ] attrib

  (* XXX *)
  val a_restart :
    [< | `Always | `WhenNotActive | `Never ] wrap -> [> | `Restart ] attrib

  val a_repeatCount : string wrap -> [> | `RepeatCount ] attrib

  (* XXX *)
  val a_repeatDur : string wrap -> [> | `RepeatDur ] attrib

  (* XXX *)
  val a_fill : paint wrap -> [> | `Fill ] attrib

  val a_animation_fill : [< | `Freeze | `Remove ] wrap -> [> | `Fill_Animation ] attrib

  val a_calcMode :
    [< | `Discrete | `Linear | `Paced | `Spline ] wrap -> [> | `CalcMode ] attrib

  val a_animation_values : strings wrap -> [> | `Valuesanim ] attrib

  val a_keyTimes : strings wrap -> [> | `KeyTimes ] attrib

  val a_keySplines : strings wrap -> [> | `KeySplines ] attrib

  val a_from : string wrap -> [> | `From ] attrib

  val a_to : string wrap -> [> | `To ] attrib

  val a_by : string wrap -> [> | `By ] attrib

  val a_additive : [< | `Replace | `Sum ] wrap -> [> | `Additive ] attrib

  val a_accumulate : [< | `None | `Sum ] wrap -> [> | `Accumulate ] attrib

  val a_keyPoints : numbers_semicolon wrap -> [> | `KeyPoints ] attrib

  val a_path : string wrap -> [> | `Path ] attrib

  val a_animateColor_type :
    [ | `Translate | `Scale | `Rotate | `SkewX | `SkewY ] wrap ->
    [ | `Typeanimatecolor ] attrib

  val a_horiz_origin_x : number wrap -> [> | `HorizOriginX ] attrib

  val a_horiz_origin_y : number wrap -> [> | `HorizOriginY ] attrib

  val a_horiz_adv_x : number wrap -> [> | `HorizAdvX ] attrib

  val a_vert_origin_x : number wrap -> [> | `VertOriginX ] attrib

  val a_vert_origin_y : number wrap -> [> | `VertOriginY ] attrib

  val a_vert_adv_y : number wrap -> [> | `VertAdvY ] attrib

  val a_unicode : string wrap -> [> | `Unicode ] attrib

  val a_glyph_name : string wrap -> [> | `glyphname ] attrib

  val a_orientation : [< | `H | `V ] wrap -> [> | `Orientation ] attrib

  val a_arabic_form :
    [< | `Initial | `Medial | `Terminal | `Isolated ] wrap ->
    [> | `Arabicform ] attrib

  val a_lang : string wrap -> [> | `Lang ] attrib

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

  val a_stemv : number wrap -> [> | `Stemv ] attrib

  val a_stemh : number wrap -> [> | `Stemh ] attrib

  val a_slope : number wrap -> [> | `Slope ] attrib

  val a_cap_height : number wrap -> [> | `CapHeight ] attrib

  val a_x_height : number wrap -> [> | `XHeight ] attrib

  val a_accent_height : number wrap -> [> | `AccentHeight ] attrib

  val a_ascent : number wrap -> [> | `Ascent ] attrib

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
       | `Alphabetic | `Hanging | `Mathematical | `Inherit ] wrap ->
    [> | `Alignment_Baseline ] attrib

  val a_dominant_baseline :
    [< | `Auto | `Use_script | `No_change | `Reset_size | `Ideographic
       | `Alphabetic | `Hanging | `Mathematical | `Central | `Middle
       | `Text_after_edge | `Text_before_edge | `Inherit ] wrap ->
    [> | `Dominant_Baseline ] attrib

  (** Javascript events *)

  val a_onabort : Xml.event_handler  -> [> | `OnAbort ] attrib
  val a_onactivate : Xml.event_handler  -> [> | `OnActivate ] attrib
  val a_onbegin : Xml.event_handler  -> [> | `OnBegin ] attrib
  val a_onend : Xml.event_handler  -> [> | `OnEnd ] attrib
  val a_onerror : Xml.event_handler  -> [> | `OnError ] attrib
  val a_onfocusin : Xml.event_handler  -> [> | `OnFocusIn ] attrib
  val a_onfocusout : Xml.event_handler  -> [> | `OnFocusOut ] attrib
  val a_onload : Xml.event_handler  -> [> | `OnLoad ] attrib
  val a_onrepeat : Xml.event_handler  -> [> | `OnRepeat ] attrib
  val a_onresize : Xml.event_handler  -> [> | `OnResize ] attrib
  val a_onscroll : Xml.event_handler  -> [> | `OnScroll ] attrib
  val a_onunload : Xml.event_handler  -> [> | `OnUnload ] attrib
  val a_onzoom : Xml.event_handler  -> [> | `OnZoom ] attrib

  (** Javascript mouse events *)

  val a_onclick : Xml.mouse_event_handler  -> [> | `OnClick ] attrib
  val a_onmousedown : Xml.mouse_event_handler  -> [> | `OnMouseDown ] attrib
  val a_onmouseup : Xml.mouse_event_handler  -> [> | `OnMouseUp ] attrib
  val a_onmouseover : Xml.mouse_event_handler  -> [> | `OnMouseOver ] attrib
  val a_onmouseout : Xml.mouse_event_handler  -> [> | `OnMouseOut ] attrib
  val a_onmousemove : Xml.mouse_event_handler  -> [> | `OnMouseMove ] attrib


  val metadata :
    ?a: ((metadata_attr attrib) list) -> Xml.elt list_wrap -> [> | metadata] elt

  val foreignObject :
    ?a: ((foreignobject_attr attrib) list) ->
    Xml.elt list_wrap -> [> | foreignobject] elt

  val a_stop_color : color wrap -> [> | `Stop_Color ] attrib

  val a_stop_opacity : number wrap -> [> | `Stop_Opacity ] attrib

  val a_stroke : paint wrap -> [> | `Stroke ] attrib

  val a_stroke_width : length wrap -> [> | `Stroke_Width ] attrib

  val a_stroke_linecap :
    [< `Butt | `Round | `Square ] wrap -> [> | `Stroke_Linecap ] attrib

  val a_stroke_linejoin :
    [< `Miter | `Round | `Bever ] wrap -> [> `Stroke_Linejoin ] attrib

  val a_stroke_miterlimit : float wrap -> [> `Stroke_Miterlimit ] attrib

  val a_stroke_dasharray :
    Unit.length list wrap -> [> `Stroke_Dasharray ] attrib

  val a_stroke_dashoffset : Unit.length wrap -> [> `Stroke_Dashoffset ] attrib

  val a_stroke_opacity : float wrap -> [> `Stroke_Opacity ] attrib

  (** {1 Elements} *)

  (* generated *)
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

  val textpath :
    ([< | textpath_attr], [< | textpath_content], [> | textpath]) star

  val altglyph :
    ([< | altglyph_attr], [< | altglyph_content], [> | altglyph]) unary

  val altglyphdef :
    ([< | altglyphdef_attr], [< | altglyphdef_content], [> | altglyphdef])
      unary

  (* theoretically a plus, simplified into star *)
  val altglyphitem :
    ([< | altglyphitem_attr], [< | altglyphitem_content], [> | altglyphitem
                                                          ]) star

  val glyphref : ([< | glyphref_attr], [> | glyphref]) nullary

  val marker : ([< | marker_attr], [< | marker_content], [> | marker]) star

  val colorprofile :
    ([< | colorprofile_attr], [< | colorprofile_content], [> | colorprofile
                                                          ]) star

  val lineargradient :
    ([< | lineargradient_attr], [< | lineargradient_content],
     [> | lineargradient]) star

  val radialgradient :
    ([< | radialgradient_attr], [< | radialgradient_content],
     [> | radialgradient]) star

  val stop :
    ([< | stop_attr], [< | stop_content], [> | stop ]) star

  val pattern :
    ([< | pattern_attr], [< | pattern_content], [> | pattern]) star

  val clippath :
    ([< | clippath_attr], [< | clippath_content], [> | clippath]) star

  val filter : ([< | filter_attr], [< | filter_content], [> | filter]) star

  val fedistantlight :
    ([< | fedistantlight_attr], [< | fedistantlight_content],
     [> | fedistantlight]) star

  val fepointlight :
    ([< | fepointlight_attr], [< | fepointlight_content], [> | fepointlight
                                                          ]) star

  val fespotlight :
    ([< | fespotlight_attr], [< | fespotlight_content], [> | fespotlight])
      star

  val feblend :
    ([< | feblend_attr], [< | feblend_content], [> | feblend]) star

  val fecolormatrix :
    ([< | fecolormatrix_attr], [< | fecolormatrix_content],
     [> | fecolormatrix]) star

  val fecomponenttransfer :
    ([< | fecomponenttransfer_attr], [< | fecomponenttransfer_content],
     [> | fecomponenttransfer]) star

  val fefunca :
    ([< | fefunca_attr], [< | fefunca_content], [> | fefunca]) star

  val fefuncg :
    ([< | fefuncg_attr], [< | fefuncg_content], [> | fefuncg]) star

  val fefuncb :
    ([< | fefuncb_attr], [< | fefuncb_content], [> | fefuncb]) star

  val fefuncr :
    ([< | fefuncr_attr], [< | fefuncr_content], [> | fefuncr]) star

  val fecomposite :
    ([< | fecomposite_attr], [< | fecomposite_content], [> | fecomposite])
      star

  val feconvolvematrix :
    ([< | feconvolvematrix_attr], [< | feconvolvematrix_content],
     [> | feconvolvematrix]) star

  val fediffuselighting :
    ([< | fediffuselighting_attr], [< | fediffuselighting_content],
     [> | fediffuselighting]) star

  val fedisplacementmap :
    ([< | fedisplacementmap_attr], [< | fedisplacementmap_content],
     [> | fedisplacementmap]) star

  val feflood :
    ([< | feflood_attr], [< | feflood_content], [> | feflood]) star

  val fegaussianblur :
    ([< | fegaussianblur_attr], [< | fegaussianblur_content],
     [> | fegaussianblur]) star

  val feimage :
    ([< | feimage_attr], [< | feimage_content], [> | feimage]) star

  val femerge :
    ([< | femerge_attr], [< | femerge_content], [> | femerge]) star

  val femorphology :
    ([< | femorphology_attr], [< | femorphology_content], [> | femorphology
                                                          ]) star

  val feoffset :
    ([< | feoffset_attr], [< | feoffset_content], [> | feoffset]) star

  val fespecularlighting :
    ([< | fespecularlighting_attr], [< | fespecularlighting_content],
     [> | fespecularlighting]) star

  val fetile : ([< | fetile_attr], [< | fetile_content], [> | fetile]) star

  val feturbulence :
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

  val animatemotion :
    ([< | animatemotion_attr], [< | animatemotion_content],
     [> | animatemotion]) star

  val mpath : ([< | mpath_attr], [< | mpath_content], [> | mpath]) star

  val animatecolor :
    ([< | animatecolor_attr], [< | animatecolor_content], [> | animatecolor
                                                          ]) star

  val animatetransform :
    ([< | animatetransform_attr], [< | animatetransform_content],
     [> | animatetransform]) star

  val font : ([< | font_attr], [< | font_content], [> | font]) star

  val glyph : ([< | glyph_attr], [< | glyph_content], [> | glyph]) star

  val missingglyph :
    ([< | missingglyph_attr], [< | missingglyph_content], [> | missingglyph
                                                          ]) star

  val hkern : ([< | hkern_attr], [> | hkern]) nullary

  val vkern : ([< | vkern_attr], [> | vkern]) nullary

  val font_face : ([< | font_face_attr], [> | font_face]) nullary

  val font_face_src :
    ([< | font_face_src_attr], [< | font_face_src_content], [> | font_face_src])
      star

  val font_face_uri :
    ([< | font_face_uri_attr], [< | font_face_uri_content], [> | font_face_uri])
      star

  val font_face_format :
    ([< | font_face_format_attr], [> | font_face_format]) nullary

  val font_face_name : ([< | font_face_name_attr], [> | font_face_name]) nullary

  val tot : Xml.elt -> 'a elt

  val totl : Xml.elt list -> ('a elt) list

  val toelt : 'a elt -> Xml.elt

  val toeltl : ('a elt) list -> Xml.elt list

  val to_xmlattribs : ('a attrib) list -> Xml.attrib list
  val to_attrib : Xml.attrib -> 'a attrib

  module Unsafe : sig
    (** Unsafe features. Warning using this module can break
        validity and may introduce security problems like
        code injection.
        Use it with care.
    *)

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

    (** Same, for a space separated list of values *)
    val space_sep_attrib : string -> string list wrap -> 'a attrib

    (** Same, for a comma separated list of values *)
    val comma_sep_attrib : string -> string list wrap -> 'a attrib

  end

  (** {2 ... } *)

  type doc = [ `Svg ] elt
  val doc_toelt : doc -> Xml.elt


end

module type NoWrap = T with module Xml.W = Xml_wrap.NoWrap

module type Wrapped_functions = sig

  type (-'a, 'b) ft

  val string_of_alignment_baseline :
    ([< Svg_types.alignment_baseline], string) ft

  val string_of_bool : (bool, string) ft

  val string_of_big_variant : ([< Svg_types.big_variant], string) ft

  val string_of_coords : (Svg_types.coords, string) ft

  val string_of_dominant_baseline :
    ([< Svg_types.dominant_baseline], string) ft

  val string_of_fourfloats : (float * float * float * float, string) ft

  val string_of_in_value : ([< Svg_types.in_value], string) ft

  val string_of_int : (int, string) ft

  val string_of_length : (Svg_types.Unit.length, string) ft

  val string_of_lengths : (Svg_types.lengths, string) ft

  val string_of_number : (float, string) ft

  val string_of_number_optional_number :
    (float * float option, string) ft

  val string_of_numbers : (float list, string) ft

  val string_of_numbers_semicolon : (float list, string) ft

  val string_of_offset : ([< Svg_types.offset], string) ft

  val string_of_orient : (Svg_types.Unit.angle option, string) ft

  val string_of_paint : ([< Svg_types.paint], string) ft

  val string_of_strokedasharray : (Svg_types.lengths, string) ft

  val string_of_transform : (Svg_types.transform, string) ft

  val string_of_transforms : (Svg_types.transforms, string) ft

end

(** {2 Signature functors} *)
(** See {% <<a_manual chapter="functors"|the manual of the functorial interface>> %}. *)

(** Signature functor for {!Svg_f.Make}. *)
module Make (Xml : Xml_sigs.T) : sig

  (** See {!modtype:Svg_sigs.T}. *)
  module type T = T
    with type 'a Xml.W.t = 'a Xml.W.t
     and type 'a Xml.W.tlist = 'a Xml.W.tlist
     and type ('a,'b) Xml.W.ft = ('a,'b) Xml.W.ft
     and type Xml.uri = Xml.uri
     and type Xml.event_handler = Xml.event_handler
     and type Xml.mouse_event_handler = Xml.mouse_event_handler
     and type Xml.keyboard_event_handler = Xml.keyboard_event_handler
     and type Xml.attrib = Xml.attrib
     and type Xml.elt = Xml.elt

end
