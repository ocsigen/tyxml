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

  module Xml : Xml_sigs.Wrapped
  module Info : Xml_sigs.Info

  type uri = Xml.uri
  val string_of_uri : uri -> string
  val uri_of_string : string -> uri

  (** {1 Abstraction over XML's types} *)

  type 'a attrib

  type 'a wrap

  type +'a elt

  type +'a elts

  type ('a, 'b) nullary = ?a: (('a attrib) list) -> unit -> 'b elt

  type ('a, 'b, 'c) unary = ?a: (('a attrib) list) -> 'b elt wrap -> 'c elt

  type ('a, 'b, 'c) star =
      ?a: (('a attrib) list) -> ('b elt) list wrap -> 'c elt

  type ('a, 'b, 'c) plus =
      ?a: (('a attrib) list) -> 'b elt wrap -> ('b elt) list wrap -> 'c elt

  (* to be processed by a script *)
  type altglyphdef_content =
    [ | `Ref of (glyphref elt) list | `Item of (altglyphitem elt) list
    ]

  val pcdata : string wrap -> [> | `PCDATA] elt

  (** {1 attributes } *)

  val a_version : string wrap -> [> | `Version ] attrib

  val a_baseprofile : string wrap -> [> | `BaseProfile ] attrib

  val a_x : coord wrap -> [> | `X ] attrib

  val a_y : coord wrap -> [> | `Y ] attrib

  val a_width : Unit.length wrap -> [> | `Width ] attrib

  val a_height : Unit.length wrap -> [> | `Height ] attrib

  val a_preserveaspectratio : string wrap -> [> | `PreserveAspectRatio ] attrib

  val a_contentscripttype : string wrap -> [> | `ContentScriptType ] attrib

  val a_contentstyletype : string wrap -> [> | `ContentStyleType ] attrib

  val a_zoomAndPan : [< | `Disable | `Magnify ] wrap -> [> | `ZoomAndSpan ] attrib

  val a_xlink_href : iri wrap -> [> | `Xlink_href ] attrib

  val a_requiredfeatures : spacestrings wrap -> [> | `RequiredFeatures ] attrib

  val a_requiredextensions :
    spacestrings wrap -> [> | `RequiredExtension ] attrib

  val a_systemlanguage : commastrings wrap -> [> | `SystemLanguage ] attrib

  val a_externalressourcesrequired :
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

  val a_viewbox : fourfloats wrap -> [> | `ViewBox ] attrib

  val a_d : string wrap -> [> | `D ] attrib

  val a_pathlength : float wrap -> [> | `PathLength ] attrib

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

  val a_lengthadjust :
    [< `Spacing | `SpacingAndGlyphs ] wrap -> [> | `LengthAdjust ] attrib

  val a_textlength : Unit.length wrap -> [> | `TextLength ] attrib

  val a_rotate : numbers wrap -> [> | `Rotate ] attrib

  val a_startoffset : Unit.length wrap -> [> | `StartOffset ] attrib

  val a_method : [< `Align | `Stretch ] wrap -> [> | `Method ] attrib

  val a_spacing : [< `Auto | `Exact ] wrap -> [> | `Spacing ] attrib

  val a_glyphref : string wrap -> [> | `GlyphRef ] attrib

  val a_format : string wrap -> [> | `Format ] attrib

  val a_markerunits :
    [< `StrokeWidth | `UserSpaceOnUse ] wrap -> [> | `MarkerUnits ] attrib

  val a_refx : coord wrap -> [> | `RefX ] attrib

  val a_refy : coord wrap -> [> | `RefY ] attrib

  val a_markerwidth : Unit.length wrap -> [> | `MarkerWidth ] attrib

  val a_markerheight : Unit.length wrap -> [> | `MarkerHeight ] attrib

  val a_orient : [< `Auto | `Angle of angle ] wrap -> [> | `Orient ] attrib

  val a_local : string wrap -> [> | `Local ] attrib

  val a_renderingindent :
    [<
    | `Auto
    | `Perceptual
    | `Relative_colorimetric
    | `Saturation
    | `Absolute_colorimetric ] wrap -> [> | `Rendering_Indent ] attrib

  val a_gradientunits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
    [ | `GradientUnits ] attrib

  val a_gradienttransform : transforms wrap -> [> | `Gradient_Transform ] attrib

  val a_spreadmethod :
    [< `Pad | `Reflect | `Repeat ] wrap -> [> | `SpreadMethod ] attrib

  val a_fx : coord wrap -> [> | `Fx ] attrib

  val a_fy : coord wrap -> [> | `Fy ] attrib

  val a_offset :
    [< `Number of number | `Percentage of percentage ] wrap ->
    [> | `Offset ] attrib

  val a_patternunits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
    [> | `PatternUnits ] attrib

  val a_patterncontentunits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
    [> | `PatternContentUnits ] attrib

  val a_patterntransform : transforms wrap -> [> | `PatternTransform ] attrib

  val a_clippathunits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
    [> | `ClipPathUnits ] attrib

  val a_maskunits :
    [< | `UserSpaceOnUse | `ObjectBoundingBox ] wrap -> [> | `MaskUnits ] attrib

  val a_maskcontentunits :
    [< | `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
    [> | `MaskContentUnits ] attrib

  val a_primitiveunits :
    [< | `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
    [> | `PrimitiveUnits ] attrib

  val a_filterres : number_optional_number wrap -> [> | `FilterResUnits ] attrib

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

  val a_aizmuth : float wrap -> [> | `Azimuth ] attrib

  val a_elevation : float wrap -> [> | `Elevation ] attrib

  val a_pointatx : float wrap -> [> | `PointsAtX ] attrib

  val a_pointaty : float wrap -> [> | `PointsAtY ] attrib

  val a_pointatz : float wrap -> [> | `PointsAtZ ] attrib

  val a_specularexponent : float wrap -> [> | `SpecularExponent ] attrib

  val a_specularconstant : float wrap -> [> | `SpecularConstant ] attrib

  val a_limitingconeangle : float wrap -> [> | `LimitingConeAngle ] attrib

  val a_mode :
    [< | `Normal | `Multiply | `Screen | `Darken | `Lighten ] wrap ->
    [> | `Mode ] attrib

  val a_typefecolor :
    [< | `Matrix | `Saturate | `HueRotate | `LuminanceToAlpha ] wrap ->
    [> | `Typefecolor ] attrib

  val a_values : numbers wrap -> [> | `Values ] attrib

  val a_transferttype :
    [< | `Identity | `Table | `Discrete | `Linear | `Gamma ] wrap ->
    [> | `Typetransfert ] attrib

  val a_tablevalues : numbers wrap -> [> | `TableValues ] attrib

  val a_slope : number wrap -> [> | `Slope ] attrib

  val a_intercept : number wrap -> [> | `Intercept ] attrib

  val a_amplitude : number wrap -> [> | `Amplitude ] attrib

  val a_exponent : number wrap -> [> | `Exponent ] attrib

  val a_offsettransfer : number wrap -> [> | `Offsettransfer ] attrib

  val a_operator :
    [< | `Over | `In | `Out | `Atop | `Xor | `Arithmetic ] wrap ->
    [> | `Operator ] attrib

  val a_k1 : number wrap -> [> | `K1 ] attrib

  val a_k2 : number wrap -> [> | `K2 ] attrib

  val a_k3 : number wrap -> [> | `K3 ] attrib

  val a_k4 : number wrap -> [> | `K4 ] attrib

  val a_order : number_optional_number wrap -> [> | `Order ] attrib

  val a_kernelmatrix : numbers wrap -> [> | `KernelMatrix ] attrib

  val a_divisor : number wrap -> [> | `Divisor ] attrib

  val a_bias : number wrap -> [> | `Bias ] attrib

  val a_kernelunitlength :
    number_optional_number wrap -> [> | `KernelUnitLength ] attrib

  val a_targetX : int wrap -> [> | `TargetX ] attrib

  val a_targetY : int wrap -> [> | `TargetY ] attrib

  val a_edgemode :
    [< | `Duplicate | `Wrap | `None ] wrap -> [> | `TargetY ] attrib

  val a_preservealpha : bool wrap -> [> | `TargetY ] attrib

  val a_surfacescale : number wrap -> [> | `SurfaceScale ] attrib

  val a_diffuseconstant : number wrap -> [> | `DiffuseConstant ] attrib

  val a_scale : number wrap -> [> | `Scale ] attrib

  val a_xchannelselector :
    [< | `R | `G | `B | `A ] wrap -> [> | `XChannelSelector ] attrib

  val a_ychannelselector :
    [< | `R | `G | `B | `A ] wrap -> [> | `YChannelSelector ] attrib

  val a_stddeviation : number_optional_number wrap -> [> | `StdDeviation ] attrib

  val a_operatormorphology :
    [< | `Erode | `Dilate ] wrap -> [> | `OperatorMorphology ] attrib

  val a_radius : number_optional_number wrap -> [> | `Radius ] attrib

  val a_basefrenquency :
    number_optional_number wrap -> [> | `BaseFrequency ] attrib

  val a_numoctaves : int wrap -> [> | `NumOctaves ] attrib

  val a_seed : number wrap -> [> | `Seed ] attrib

  val a_stitchtiles :
    [< | `Stitch | `NoStitch ] wrap -> [> | `StitchTiles ] attrib

  val a_stitchtype :
    [< | `FractalNoise | `Turbulence ] wrap -> [> | `TypeStitch ] attrib

  val a_xlinkshow : [< | `New | `Replace ] wrap -> [> | `Xlink_show ] attrib

  val a_xlinkactuate : [< | `OnRequest ] wrap -> [> | `Xlink_actuate ] attrib

  val a_target : string wrap -> [> | `Xlink_target ] attrib

  val a_viewtarget : string wrap -> [> | `ViewTarget ] attrib

  val a_attributename : string wrap -> [> | `AttributeName ] attrib

  val a_attributetype :
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

  val a_repeatcount : string wrap -> [> | `RepeatCount ] attrib

  (* XXX *)
  val a_repeatdur : string wrap -> [> | `RepeatDur ] attrib

  (* XXX *)
  val a_fill : paint wrap -> [> | `Fill ] attrib

  val a_fill_animation : [< | `Freeze | `Remove ] wrap -> [> | `Fill_Animation ] attrib

  val a_calcmode :
    [< | `Discrete | `Linear | `Paced | `Spline ] wrap -> [> | `CalcMode ] attrib

  val a_values_anim : strings wrap -> [> | `Valuesanim ] attrib

  val a_keytimes : strings wrap -> [> | `KeyTimes ] attrib

  val a_keysplines : strings wrap -> [> | `KeySplines ] attrib

  val a_from : string wrap -> [> | `From ] attrib

  val a_to : string wrap -> [> | `To ] attrib

  val a_by : string wrap -> [> | `By ] attrib

  val a_additive : [< | `Replace | `Sum ] wrap -> [> | `Additive ] attrib

  val a_accumulate : [< | `None | `Sum ] wrap -> [> | `Accumulate ] attrib

  val a_keypoints : numbers_semicolon wrap -> [> | `KeyPoints ] attrib

  val a_path : string wrap -> [> | `Path ] attrib

  val a_typeanimatecolor :
    [ | `Translate | `Scale | `Rotate | `SkewX | `SkewY ] wrap ->
    [ | `Typeanimatecolor ] attrib

  val a_horiz_origin_x : number wrap -> [> | `Horizoriginx ] attrib

  val a_horiz_origin_y : number wrap -> [> | `Horizoriginy ] attrib

  val a_horiz_adv_x : number wrap -> [> | `Horizadvx ] attrib

  val a_vert_origin_x : number wrap -> [> | `Vertoriginx ] attrib

  val a_vert_origin_y : number wrap -> [> | `Vertoriginy ] attrib

  val a_vert_adv_y : number wrap -> [> | `Vertadvy ] attrib

  val a_unicode : string wrap -> [> | `Unicode ] attrib

  val a_glyphname : string wrap -> [> | `glyphname ] attrib

  val a_orientation : [< | `H | `V ] wrap -> [> | `Orientation ] attrib

  val a_arabicform :
    [< | `Initial | `Medial | `Terminal | `Isolated ] wrap ->
    [> | `Arabicform ] attrib

  val a_lang : string wrap -> [> | `Lang ] attrib

  val a_u1 : string wrap -> [> | `U1 ] attrib

  val a_u2 : string wrap -> [> | `U2 ] attrib

  val a_g1 : string wrap -> [> | `G1 ] attrib

  val a_g2 : string wrap -> [> | `G2 ] attrib

  val a_k : string wrap -> [> | `K ] attrib

  val a_fontfamily : string wrap -> [> | `FontFamily ] attrib

  val a_fontstyle : string wrap -> [> | `FontStyle ] attrib

  val a_fontvariant : string wrap -> [> | `FontVariant ] attrib

  val a_fontweight : string wrap -> [> | `FontWeight ] attrib

  val a_fontstretch : string wrap -> [> | `FontStretch ] attrib

  val a_fontsize : string wrap -> [> | `FontSize ] attrib

  val a_unicoderange : string wrap -> [> | `UnicodeRange ] attrib

  val a_unitsperem : string wrap -> [> | `UnitsPerEm ] attrib

  val a_stemv : number wrap -> [> | `Stemv ] attrib

  val a_stemh : number wrap -> [> | `Stemh ] attrib

  val a_slope : number wrap -> [> | `Slope ] attrib

  val a_capheight : number wrap -> [> | `CapHeight ] attrib

  val a_xheight : number wrap -> [> | `XHeight ] attrib

  val a_accentheight : number wrap -> [> | `AccentHeight ] attrib

  val a_ascent : number wrap -> [> | `Ascent ] attrib

  val a_widths : string wrap -> [> | `Widths ] attrib

  val a_bbox : string wrap -> [> | `Bbox ] attrib

  val a_ideographic : number wrap -> [> | `Ideographic ] attrib

  val a_alphabetic : number wrap -> [> | `Alphabetic ] attrib

  val a_mathematical : number wrap -> [> | `Mathematical ] attrib

  val a_hanging : number wrap -> [> | `Hanging ] attrib

  val a_videographic : number wrap -> [> | `VIdeographic ] attrib

  val a_valphabetic : number wrap -> [> | `VAlphabetic ] attrib

  val a_vmathematical : number wrap -> [> | `VMathematical ] attrib

  val a_vhanging : number wrap -> [> | `VHanging ] attrib

  val a_underlineposition : number wrap -> [> | `UnderlinePosition ] attrib

  val a_underlinethickness : number wrap -> [> | `UnderlineThickness ] attrib

  val a_strikethroughposition :
    number wrap -> [> | `StrikethroughPosition ] attrib

  val a_strikethroughthickness :
    number wrap -> [> | `StrikethroughThickness ] attrib

  val a_overlineposition : number wrap -> [> | `OverlinePosition ] attrib

  val a_overlinethickness : number wrap -> [> | `OverlineThickness ] attrib

  val a_string : string wrap -> [> | `String ] attrib

  val a_name : string wrap -> [> | `Name ] attrib

  val a_onabort : Xml.event_handler  -> [> | `OnAbort ] attrib

  val a_onactivate : Xml.event_handler  -> [> | `OnActivate ] attrib

  val a_onbegin : Xml.event_handler  -> [> | `OnBegin ] attrib

  val a_onclick : Xml.event_handler  -> [> | `OnClick ] attrib

  val a_onend : Xml.event_handler  -> [> | `OnEnd ] attrib

  val a_onerror : Xml.event_handler  -> [> | `OnError ] attrib

  val a_onfocusin : Xml.event_handler  -> [> | `OnFocusIn ] attrib

  val a_onfocusout : Xml.event_handler  -> [> | `OnFocusOut ] attrib

  val a_onload : Xml.event_handler  -> [> | `OnLoad ] attrib

  val a_onmousedown : Xml.event_handler  -> [> | `OnMouseDown ] attrib

  val a_onmouseup : Xml.event_handler  -> [> | `OnMouseUp ] attrib

  val a_onmouseover : Xml.event_handler  -> [> | `OnMouseOver ] attrib

  val a_onmouseout : Xml.event_handler  -> [> | `OnMouseOut ] attrib

  val a_onmousemove : Xml.event_handler  -> [> | `OnMouseMove ] attrib

  val a_onrepeat : Xml.event_handler  -> [> | `OnRepeat ] attrib

  val a_onresize : Xml.event_handler  -> [> | `OnResize ] attrib

  val a_onscroll : Xml.event_handler  -> [> | `OnScroll ] attrib

  val a_onunload : Xml.event_handler  -> [> | `OnUnload ] attrib

  val a_onzoom : Xml.event_handler  -> [> | `OnZoom ] attrib

  val metadata :
    ?a: ((metadata_attr attrib) list) -> Xml.elt list wrap -> [> | metadata] elt

  val foreignobject :
    ?a: ((foreignobject_attr attrib) list) ->
    Xml.elt list wrap -> [> | foreignobject] elt

  val a_stopcolor : color wrap -> [> | `Stop_Color ] attrib

  val a_stopopacity : number wrap -> [> | `Stop_Opacity ] attrib

  val a_stroke : paint wrap -> [> | `Stroke ] attrib

  val a_strokewidth : length wrap -> [> | `Stroke_Width ] attrib

  val a_strokelinecap :
    [< `Butt | `Round | `Square ] wrap -> [> | `Stroke_Linecap ] attrib

  val a_strokelinejoin :
    [< `Miter | `Round | `Bever ] wrap -> [> `Stroke_Linejoin ] attrib

  val a_strokemiterlimit : float wrap -> [> `Stroke_Miterlimit ] attrib

  val a_strokedasharray : Unit.length list wrap -> [> `Stroke_Dasharray ] attrib

  val a_strokedashoffset : Unit.length wrap -> [> `Stroke_Dashoffset ] attrib

  val a_strokeopacity : float wrap -> [> `Stroke_Opacity ] attrib

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

  val altglyphitem :
    ([< | altglyphitem_attr], [< | altglyphitem_content], [> | altglyphitem
							  ]) plus

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
    val data : string wrap -> 'a elt

    (** Insert an XML node that is not implemented in this module.
        If it is a standard SVG node which is missing,
        please report to the Ocsigen team.
    *)
    val node : string -> ?a:'a attrib list -> 'b elt list wrap -> 'c elt

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
