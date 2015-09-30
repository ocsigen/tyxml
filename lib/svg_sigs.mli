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
  val string_of_uri : uri -> string
  val uri_of_string : string -> uri

  (** {1 Abstraction over XML's types} *)

  type +'a attrib

  type 'a wrap = 'a Xml.W.t
  type 'a list_wrap = 'a Xml.W.tlist
  type 'a attr_wrap = 'a Xml.W.attr

  type +'a elt

  type ('a, 'b) nullary = ?a: (('a attrib) list) -> unit -> 'b elt

  type ('a, 'b, 'c) unary = ?a: (('a attrib) list) -> 'b elt wrap -> 'c elt

  type ('a, 'b, 'c) star =
    ?a: (('a attrib) list) -> ('b elt) list_wrap -> 'c elt

  (* to be processed by a script *)
  type altglyphdef_content =
    [ | `Ref of (glyphref elt) list | `Item of (altglyphitem elt) list
    ]

  val pcdata : string attr_wrap -> [> | `PCDATA] elt

  (** {1 attributes } *)

  val a_version : string attr_wrap -> [> | `Version ] attrib

  val a_baseprofile : string attr_wrap -> [> | `BaseProfile ] attrib

  val a_x : coord attr_wrap -> [> | `X ] attrib

  val a_y : coord attr_wrap -> [> | `Y ] attrib

  val a_width : Unit.length attr_wrap -> [> | `Width ] attrib

  val a_height : Unit.length attr_wrap -> [> | `Height ] attrib

  val a_preserveaspectratio : string attr_wrap -> [> | `PreserveAspectRatio ] attrib

  val a_contentscripttype : string attr_wrap -> [> | `ContentScriptType ] attrib

  val a_contentstyletype : string attr_wrap -> [> | `ContentStyleType ] attrib

  val a_zoomAndPan : [< | `Disable | `Magnify ] attr_wrap -> [> | `ZoomAndSpan ] attrib

  val a_xlink_href : iri attr_wrap -> [> | `Xlink_href ] attrib

  val a_requiredfeatures : spacestrings attr_wrap -> [> | `RequiredFeatures ] attrib

  val a_requiredextensions :
    spacestrings attr_wrap -> [> | `RequiredExtension ] attrib

  val a_systemlanguage : commastrings attr_wrap -> [> | `SystemLanguage ] attrib

  val a_externalressourcesrequired :
    bool attr_wrap -> [> | `ExternalRessourcesRequired ] attrib

  val a_id : string attr_wrap -> [> | `Id ] attrib

  val a_xml_base : iri attr_wrap -> [> | `Xml_Base ] attrib

  val a_xml_lang : iri attr_wrap -> [> | `Xml_Lang ] attrib

  val a_xml_space : [< `Default | `Preserve ] attr_wrap -> [> | `Xml_Space ] attrib

  val a_type : string attr_wrap -> [> | `Type ] attrib

  val a_media : commastrings attr_wrap -> [> | `Media ] attrib

  val a_title : string attr_wrap -> [> | `Title ] attrib

  val a_class : spacestrings attr_wrap -> [> | `Class ] attrib

  val a_style : string attr_wrap -> [> | `Style ] attrib

  val a_transform : transform attr_wrap -> [> | `Transform ] attrib

  val a_viewbox : fourfloats attr_wrap -> [> | `ViewBox ] attrib

  val a_d : string attr_wrap -> [> | `D ] attrib

  val a_pathlength : float attr_wrap -> [> | `PathLength ] attrib

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

  val a_y_list : lengths attr_wrap -> [> | `Y_list ] attrib

  val a_dx : number attr_wrap -> [> | `Dx ] attrib

  val a_dy : number attr_wrap -> [> | `Dy ] attrib

  val a_dx_list : lengths attr_wrap -> [> | `Dx_list ] attrib

  val a_dy_list : lengths attr_wrap -> [> | `Dy_list ] attrib

  val a_lengthadjust :
    [< `Spacing | `SpacingAndGlyphs ] attr_wrap -> [> | `LengthAdjust ] attrib

  val a_textlength : Unit.length attr_wrap -> [> | `TextLength ] attrib

  val a_text_anchor : [< `Start | `Middle | `End | `Inherit ] attr_wrap -> [> | `Text_Anchor ] attrib

  val a_text_decoration : [< `None | `Underline | `Overline | `Line_through | `Blink | `Inherit ] attr_wrap -> [> | `Text_Decoration ] attrib

  val a_text_rendering : [< `Auto | `OptimizeSpeed | `OptimizeLegibility | `GeometricPrecision | `Inherit ] attr_wrap -> [> | `Text_Rendering ] attrib

  val a_rotate : numbers attr_wrap -> [> | `Rotate ] attrib

  val a_startoffset : Unit.length attr_wrap -> [> | `StartOffset ] attrib

  val a_method : [< `Align | `Stretch ] attr_wrap -> [> | `Method ] attrib

  val a_spacing : [< `Auto | `Exact ] attr_wrap -> [> | `Spacing ] attrib

  val a_glyphref : string attr_wrap -> [> | `GlyphRef ] attrib

  val a_format : string attr_wrap -> [> | `Format ] attrib

  val a_markerunits :
    [< `StrokeWidth | `UserSpaceOnUse ] attr_wrap -> [> | `MarkerUnits ] attrib

  val a_refx : coord attr_wrap -> [> | `RefX ] attrib

  val a_refy : coord attr_wrap -> [> | `RefY ] attrib

  val a_markerwidth : Unit.length attr_wrap -> [> | `MarkerWidth ] attrib

  val a_markerheight : Unit.length attr_wrap -> [> | `MarkerHeight ] attrib

  val a_orient : [< `Auto | `Angle of angle ] attr_wrap -> [> | `Orient ] attrib

  val a_local : string attr_wrap -> [> | `Local ] attrib

  val a_renderingindent :
    [<
      | `Auto
      | `Perceptual
      | `Relative_colorimetric
      | `Saturation
      | `Absolute_colorimetric ] attr_wrap -> [> | `Rendering_Indent ] attrib

  val a_gradientunits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] attr_wrap ->
    [ | `GradientUnits ] attrib

  val a_gradienttransform : transforms attr_wrap -> [> | `Gradient_Transform ] attrib

  val a_spreadmethod :
    [< `Pad | `Reflect | `Repeat ] attr_wrap -> [> | `SpreadMethod ] attrib

  val a_fx : coord attr_wrap -> [> | `Fx ] attrib

  val a_fy : coord attr_wrap -> [> | `Fy ] attrib

  val a_offset :
    [< `Number of number | `Percentage of percentage ] attr_wrap ->
    [> | `Offset ] attrib

  val a_patternunits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] attr_wrap ->
    [> | `PatternUnits ] attrib

  val a_patterncontentunits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] attr_wrap ->
    [> | `PatternContentUnits ] attrib

  val a_patterntransform : transforms attr_wrap -> [> | `PatternTransform ] attrib

  val a_clippathunits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] attr_wrap ->
    [> | `ClipPathUnits ] attrib

  val a_maskunits :
    [< | `UserSpaceOnUse | `ObjectBoundingBox ] attr_wrap -> [> | `MaskUnits ] attrib

  val a_maskcontentunits :
    [< | `UserSpaceOnUse | `ObjectBoundingBox ] attr_wrap ->
    [> | `MaskContentUnits ] attrib

  val a_primitiveunits :
    [< | `UserSpaceOnUse | `ObjectBoundingBox ] attr_wrap ->
    [> | `PrimitiveUnits ] attrib

  val a_filterres : number_optional_number attr_wrap -> [> | `FilterResUnits ] attrib

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

  val a_aizmuth : float attr_wrap -> [> | `Azimuth ] attrib

  val a_elevation : float attr_wrap -> [> | `Elevation ] attrib

  val a_pointatx : float attr_wrap -> [> | `PointsAtX ] attrib

  val a_pointaty : float attr_wrap -> [> | `PointsAtY ] attrib

  val a_pointatz : float attr_wrap -> [> | `PointsAtZ ] attrib

  val a_specularexponent : float attr_wrap -> [> | `SpecularExponent ] attrib

  val a_specularconstant : float attr_wrap -> [> | `SpecularConstant ] attrib

  val a_limitingconeangle : float attr_wrap -> [> | `LimitingConeAngle ] attrib

  val a_mode :
    [< | `Normal | `Multiply | `Screen | `Darken | `Lighten ] attr_wrap ->
    [> | `Mode ] attrib

  val a_typefecolor :
    [< | `Matrix | `Saturate | `HueRotate | `LuminanceToAlpha ] attr_wrap ->
    [> | `Typefecolor ] attrib

  val a_values : numbers attr_wrap -> [> | `Values ] attrib

  val a_transferttype :
    [< | `Identity | `Table | `Discrete | `Linear | `Gamma ] attr_wrap ->
    [> | `Typetransfert ] attrib

  val a_tablevalues : numbers attr_wrap -> [> | `TableValues ] attrib

  val a_intercept : number attr_wrap -> [> | `Intercept ] attrib

  val a_amplitude : number attr_wrap -> [> | `Amplitude ] attrib

  val a_exponent : number attr_wrap -> [> | `Exponent ] attrib

  val a_offsettransfer : number attr_wrap -> [> | `Offsettransfer ] attrib

  val a_operator :
    [< | `Over | `In | `Out | `Atop | `Xor | `Arithmetic ] attr_wrap ->
    [> | `Operator ] attrib

  val a_k1 : number attr_wrap -> [> | `K1 ] attrib

  val a_k2 : number attr_wrap -> [> | `K2 ] attrib

  val a_k3 : number attr_wrap -> [> | `K3 ] attrib

  val a_k4 : number attr_wrap -> [> | `K4 ] attrib

  val a_order : number_optional_number attr_wrap -> [> | `Order ] attrib

  val a_kernelmatrix : numbers attr_wrap -> [> | `KernelMatrix ] attrib

  val a_divisor : number attr_wrap -> [> | `Divisor ] attrib

  val a_bias : number attr_wrap -> [> | `Bias ] attrib

  val a_kernelunitlength :
    number_optional_number attr_wrap -> [> | `KernelUnitLength ] attrib

  val a_targetX : int attr_wrap -> [> | `TargetX ] attrib

  val a_targetY : int attr_wrap -> [> | `TargetY ] attrib

  val a_edgemode :
    [< | `Duplicate | `Wrap | `None ] attr_wrap -> [> | `TargetY ] attrib

  val a_preservealpha : bool attr_wrap -> [> | `TargetY ] attrib

  val a_surfacescale : number attr_wrap -> [> | `SurfaceScale ] attrib

  val a_diffuseconstant : number attr_wrap -> [> | `DiffuseConstant ] attrib

  val a_scale : number attr_wrap -> [> | `Scale ] attrib

  val a_xchannelselector :
    [< | `R | `G | `B | `A ] attr_wrap -> [> | `XChannelSelector ] attrib

  val a_ychannelselector :
    [< | `R | `G | `B | `A ] attr_wrap -> [> | `YChannelSelector ] attrib

  val a_stddeviation : number_optional_number attr_wrap -> [> | `StdDeviation ] attrib

  val a_operatormorphology :
    [< | `Erode | `Dilate ] attr_wrap -> [> | `OperatorMorphology ] attrib

  val a_radius : number_optional_number attr_wrap -> [> | `Radius ] attrib

  val a_basefrenquency :
    number_optional_number attr_wrap -> [> | `BaseFrequency ] attrib

  val a_numoctaves : int attr_wrap -> [> | `NumOctaves ] attrib

  val a_seed : number attr_wrap -> [> | `Seed ] attrib

  val a_stitchtiles :
    [< | `Stitch | `NoStitch ] attr_wrap -> [> | `StitchTiles ] attrib

  val a_stitchtype :
    [< | `FractalNoise | `Turbulence ] attr_wrap -> [> | `TypeStitch ] attrib

  val a_xlinkshow : [< | `New | `Replace ] attr_wrap -> [> | `Xlink_show ] attrib

  val a_xlinkactuate :
    [< | `OnRequest | `OnLoad | `Other | `None ] attr_wrap
    -> [> | `Xlink_actuate ] attrib

  val a_target : string attr_wrap -> [> | `Xlink_target ] attrib

  val a_viewtarget : string attr_wrap -> [> | `ViewTarget ] attrib

  val a_attributename : string attr_wrap -> [> | `AttributeName ] attrib

  val a_attributetype :
    [< | `CSS | `XML | `Auto ] attr_wrap -> [> | `AttributeType ] attrib

  val a_begin : string attr_wrap -> [> | `Begin ] attrib

  val a_dur : string attr_wrap -> [> | `Dur ] attrib

  (* XXX*)
  val a_min : string attr_wrap -> [> | `Min ] attrib

  (* XXX *)
  val a_max : string attr_wrap -> [> | `Max ] attrib

  (* XXX *)
  val a_restart :
    [< | `Always | `WhenNotActive | `Never ] attr_wrap -> [> | `Restart ] attrib

  val a_repeatcount : string attr_wrap -> [> | `RepeatCount ] attrib

  (* XXX *)
  val a_repeatdur : string attr_wrap -> [> | `RepeatDur ] attrib

  (* XXX *)
  val a_fill : paint attr_wrap -> [> | `Fill ] attrib

  val a_fill_animation : [< | `Freeze | `Remove ] attr_wrap -> [> | `Fill_Animation ] attrib

  val a_calcmode :
    [< | `Discrete | `Linear | `Paced | `Spline ] attr_wrap -> [> | `CalcMode ] attrib

  val a_values_anim : strings attr_wrap -> [> | `Valuesanim ] attrib

  val a_keytimes : strings attr_wrap -> [> | `KeyTimes ] attrib

  val a_keysplines : strings attr_wrap -> [> | `KeySplines ] attrib

  val a_from : string attr_wrap -> [> | `From ] attrib

  val a_to : string attr_wrap -> [> | `To ] attrib

  val a_by : string attr_wrap -> [> | `By ] attrib

  val a_additive : [< | `Replace | `Sum ] attr_wrap -> [> | `Additive ] attrib

  val a_accumulate : [< | `None | `Sum ] attr_wrap -> [> | `Accumulate ] attrib

  val a_keypoints : numbers_semicolon attr_wrap -> [> | `KeyPoints ] attrib

  val a_path : string attr_wrap -> [> | `Path ] attrib

  val a_typeanimatecolor :
    [ | `Translate | `Scale | `Rotate | `SkewX | `SkewY ] attr_wrap ->
    [ | `Typeanimatecolor ] attrib

  val a_horiz_origin_x : number attr_wrap -> [> | `Horizoriginx ] attrib

  val a_horiz_origin_y : number attr_wrap -> [> | `Horizoriginy ] attrib

  val a_horiz_adv_x : number attr_wrap -> [> | `Horizadvx ] attrib

  val a_vert_origin_x : number attr_wrap -> [> | `Vertoriginx ] attrib

  val a_vert_origin_y : number attr_wrap -> [> | `Vertoriginy ] attrib

  val a_vert_adv_y : number attr_wrap -> [> | `Vertadvy ] attrib

  val a_unicode : string attr_wrap -> [> | `Unicode ] attrib

  val a_glyphname : string attr_wrap -> [> | `glyphname ] attrib

  val a_orientation : [< | `H | `V ] attr_wrap -> [> | `Orientation ] attrib

  val a_arabicform :
    [< | `Initial | `Medial | `Terminal | `Isolated ] attr_wrap ->
    [> | `Arabicform ] attrib

  val a_lang : string attr_wrap -> [> | `Lang ] attrib

  val a_u1 : string attr_wrap -> [> | `U1 ] attrib

  val a_u2 : string attr_wrap -> [> | `U2 ] attrib

  val a_g1 : string attr_wrap -> [> | `G1 ] attrib

  val a_g2 : string attr_wrap -> [> | `G2 ] attrib

  val a_k : string attr_wrap -> [> | `K ] attrib

  val a_fontfamily : string attr_wrap -> [> | `Font_Family ] attrib

  val a_fontstyle : string attr_wrap -> [> | `Font_Style ] attrib

  val a_fontvariant : string attr_wrap -> [> | `Font_Variant ] attrib

  val a_fontweight : string attr_wrap -> [> | `Font_Weight ] attrib

  val a_fontstretch : string attr_wrap -> [> | `Font_Stretch ] attrib

  val a_fontsize : string attr_wrap -> [> | `Font_Size ] attrib

  val a_unicoderange : string attr_wrap -> [> | `UnicodeRange ] attrib

  val a_unitsperem : string attr_wrap -> [> | `UnitsPerEm ] attrib

  val a_stemv : number attr_wrap -> [> | `Stemv ] attrib

  val a_stemh : number attr_wrap -> [> | `Stemh ] attrib

  val a_slope : number attr_wrap -> [> | `Slope ] attrib

  val a_capheight : number attr_wrap -> [> | `CapHeight ] attrib

  val a_xheight : number attr_wrap -> [> | `XHeight ] attrib

  val a_accentheight : number attr_wrap -> [> | `AccentHeight ] attrib

  val a_ascent : number attr_wrap -> [> | `Ascent ] attrib

  val a_widths : string attr_wrap -> [> | `Widths ] attrib

  val a_bbox : string attr_wrap -> [> | `Bbox ] attrib

  val a_ideographic : number attr_wrap -> [> | `Ideographic ] attrib

  val a_alphabetic : number attr_wrap -> [> | `Alphabetic ] attrib

  val a_mathematical : number attr_wrap -> [> | `Mathematical ] attrib

  val a_hanging : number attr_wrap -> [> | `Hanging ] attrib

  val a_videographic : number attr_wrap -> [> | `VIdeographic ] attrib

  val a_valphabetic : number attr_wrap -> [> | `VAlphabetic ] attrib

  val a_vmathematical : number attr_wrap -> [> | `VMathematical ] attrib

  val a_vhanging : number attr_wrap -> [> | `VHanging ] attrib

  val a_underlineposition : number attr_wrap -> [> | `UnderlinePosition ] attrib

  val a_underlinethickness : number attr_wrap -> [> | `UnderlineThickness ] attrib

  val a_strikethroughposition :
    number attr_wrap -> [> | `StrikethroughPosition ] attrib

  val a_strikethroughthickness :
    number attr_wrap -> [> | `StrikethroughThickness ] attrib

  val a_overlineposition : number attr_wrap -> [> | `OverlinePosition ] attrib

  val a_overlinethickness : number attr_wrap -> [> | `OverlineThickness ] attrib

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

  val foreignobject :
    ?a: ((foreignobject_attr attrib) list) ->
    Xml.elt list_wrap -> [> | foreignobject] elt

  val a_stopcolor : color attr_wrap -> [> | `Stop_Color ] attrib

  val a_stopopacity : number attr_wrap -> [> | `Stop_Opacity ] attrib

  val a_stroke : paint attr_wrap -> [> | `Stroke ] attrib

  val a_strokewidth : length attr_wrap -> [> | `Stroke_Width ] attrib

  val a_strokelinecap :
    [< `Butt | `Round | `Square ] attr_wrap -> [> | `Stroke_Linecap ] attrib

  val a_strokelinejoin :
    [< `Miter | `Round | `Bever ] attr_wrap -> [> `Stroke_Linejoin ] attrib

  val a_strokemiterlimit : float attr_wrap -> [> `Stroke_Miterlimit ] attrib

  val a_strokedasharray : Unit.length list attr_wrap -> [> `Stroke_Dasharray ] attrib

  val a_strokedashoffset : Unit.length attr_wrap -> [> `Stroke_Dashoffset ] attrib

  val a_strokeopacity : float attr_wrap -> [> `Stroke_Opacity ] attrib

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

  val fontface : ([< | fontface_attr], [> | fontface]) nullary

  val fontfacesrc :
    ([< | fontfacesrc_attr], [< | fontfacesrc_content], [> | fontfacesrc])
      star

  val fontfaceuri :
    ([< | fontfaceuri_attr], [< | fontfaceuri_content], [> | fontfaceuri])
      star

  val fontfaceformat :
    ([< | fontfaceformat_attr], [> | fontfaceformat]) nullary

  val fontfacename : ([< | fontfacename_attr], [> | fontfacename]) nullary

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
    val data : string attr_wrap -> 'a elt

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

  val string_of_orient : ([< Svg_types.orient], string) ft

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
     and type 'a Xml.W.attr = 'a Xml.W.attr
     and type Xml.uri = Xml.uri
     and type Xml.event_handler = Xml.event_handler
     and type Xml.mouse_event_handler = Xml.mouse_event_handler
     and type Xml.keyboard_event_handler = Xml.keyboard_event_handler
     and type Xml.attrib = Xml.attrib
     and type Xml.elt = Xml.elt

end
