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

  type 'a attrib

  type +'a elt

  type +'a elts

  type ('a, 'b) nullary = ?a: (('a attrib) list) -> unit -> 'b elt

  type ('a, 'b, 'c) unary = ?a: (('a attrib) list) -> 'b elt -> 'c elt

  type ('a, 'b, 'c) star =
      ?a: (('a attrib) list) -> ('b elt) list -> 'c elt

  type ('a, 'b, 'c) plus =
      ?a: (('a attrib) list) -> 'b elt -> ('b elt) list -> 'c elt

  (* to be processed by a script *)
  type altglyphdef_content =
    [ | `Ref of (glyphref elt) list | `Item of (altglyphitem elt) list
    ]

  (** {1 attributes } *)

  val a_version : string -> [> | `Version ] attrib

  val a_baseprofile : string -> [> | `BaseProfile ] attrib

  val a_x : coord -> [> | `X ] attrib

  val a_y : coord -> [> | `Y ] attrib

  val a_width : Unit.length -> [> | `Width ] attrib

  val a_height : Unit.length -> [> | `Height ] attrib

  val a_preserveaspectratio : string -> [> | `PreserveAspectRatio ] attrib

  val a_contentscripttype : string -> [> | `ContentScriptType ] attrib

  val a_contentstyletype : string -> [> | `ContentStyleType ] attrib

  val a_zoomAndPan : [< | `Disable | `Magnify ] -> [> | `ZoomAndSpan ] attrib

  val a_xlink_href : iri -> [> | `Xlink_href ] attrib

  val a_requiredfeatures : spacestrings -> [> | `RequiredFeatures ] attrib

  val a_requiredextensions :
    spacestrings -> [> | `RequiredExtension ] attrib

  val a_systemlanguage : commastrings -> [> | `SystemLanguage ] attrib

  val a_externalressourcesrequired :
    bool -> [> | `ExternalRessourcesRequired ] attrib

  val a_id : string -> [> | `Id ] attrib

  val a_xml_base : iri -> [> | `Xml_Base ] attrib

  val a_xml_lang : iri -> [> | `Xml_Lang ] attrib

  val a_xml_space : [< `Default | `Preserve ] -> [> | `Xml_Space ] attrib

  val a_type : string -> [> | `Type ] attrib

  val a_media : commastrings -> [> | `Media ] attrib

  val a_title : string -> [> | `Title ] attrib

  val a_class : spacestrings -> [> | `Class ] attrib

  val a_style : string -> [> | `Style ] attrib

  val a_transform : transform -> [> | `Transform ] attrib

  val a_viewbox : fourfloats -> [> | `ViewBox ] attrib

  val a_d : string -> [> | `D ] attrib

  val a_pathlength : float -> [> | `PathLength ] attrib

  (* XXX: better language support *)
  val a_rx : Unit.length -> [> | `Rx ] attrib

  val a_ry : Unit.length -> [> | `Ry ] attrib

  val a_cx : Unit.length -> [> | `Cx ] attrib

  val a_cy : Unit.length -> [> | `Cy ] attrib

  val a_r : Unit.length -> [> | `R ] attrib

  val a_x1 : coord -> [> | `X1 ] attrib

  val a_y1 : coord -> [> | `Y1 ] attrib

  val a_x2 : coord -> [> | `X2 ] attrib

  val a_y2 : coord -> [> | `Y2 ] attrib

  val a_points : coords -> [> | `Points ] attrib

  val a_x_list : lengths -> [> | `X_list ] attrib

  val a_y_list : lengths -> [> | `Y_list ] attrib

  val a_dx : number -> [> | `Dx ] attrib

  val a_dy : number -> [> | `Dy ] attrib

  val a_dx_list : lengths -> [> | `Dx_list ] attrib

  val a_dy_list : lengths -> [> | `Dy_list ] attrib

  val a_lengthadjust :
    [< `Spacing | `SpacingAndGlyphs ] -> [> | `LengthAdjust ] attrib

  val a_textlength : Unit.length -> [> | `TextLength ] attrib

  val a_rotate : numbers -> [> | `Rotate ] attrib

  val a_startoffset : Unit.length -> [> | `StartOffset ] attrib

  val a_method : [< `Align | `Stretch ] -> [> | `Method ] attrib

  val a_spacing : [< `Auto | `Exact ] -> [> | `Spacing ] attrib

  val a_glyphref : string -> [> | `GlyphRef ] attrib

  val a_format : string -> [> | `Format ] attrib

  val a_markerunits :
    [< `StrokeWidth | `UserSpaceOnUse ] -> [> | `MarkerUnits ] attrib

  val a_refx : coord -> [> | `RefX ] attrib

  val a_refy : coord -> [> | `RefY ] attrib

  val a_markerwidth : Unit.length -> [> | `MarkerWidth ] attrib

  val a_markerheight : Unit.length -> [> | `MarkerHeight ] attrib

  val a_orient : [< `Auto | `Angle of angle ] -> [> | `Orient ] attrib

  val a_local : string -> [> | `Local ] attrib

  val a_renderingindent :
    [<
    | `Auto
    | `Perceptual
    | `Relative_colorimetric
    | `Saturation
    | `Absolute_colorimetric ] -> [> | `Rendering_Indent ] attrib

  val a_gradientunits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] ->
    [ | `GradientUnits ] attrib

  val a_gradienttransform : transforms -> [> | `Gradient_Transform ] attrib

  val a_spreadmethod :
    [< `Pad | `Reflect | `Repeat ] -> [> | `SpreadMethod ] attrib

  val a_fx : coord -> [> | `Fx ] attrib

  val a_fy : coord -> [> | `Fy ] attrib

  val a_offset :
    [< `Number of number | `Percentage of percentage ] ->
    [> | `Offset ] attrib

  val a_patternunits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] ->
    [> | `PatternUnits ] attrib

  val a_patterncontentunits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] ->
    [> | `PatternContentUnits ] attrib

  val a_patterntransform : transforms -> [> | `PatternTransform ] attrib

  val a_clippathunits :
    [< `UserSpaceOnUse | `ObjectBoundingBox ] ->
    [> | `ClipPathUnits ] attrib

  val a_maskunits :
    [< | `UserSpaceOnUse | `ObjectBoundingBox ] -> [> | `MaskUnits ] attrib

  val a_maskcontentunits :
    [< | `UserSpaceOnUse | `ObjectBoundingBox ] ->
    [> | `MaskContentUnits ] attrib

  val a_primitiveunits :
    [< | `UserSpaceOnUse | `ObjectBoundingBox ] ->
    [> | `PrimitiveUnits ] attrib

  val a_filterres : number_optional_number -> [> | `FilterResUnits ] attrib

  val a_result : string -> [> | `Result ] attrib

  val a_in :
    [<
    | `SourceGraphic
    | `SourceAlpha
    | `BackgroundImage
    | `BackgroundAlpha
    | `FillPaint
    | `StrokePaint
    | `Ref of string ] -> [> | `In ] attrib

  val a_in2 :
    [<
    | `SourceGraphic
    | `SourceAlpha
    | `BackgroundImage
    | `BackgroundAlpha
    | `FillPaint
    | `StrokePaint
    | `Ref of string ] -> [> | `In2 ] attrib

  val a_aizmuth : float -> [> | `Azimuth ] attrib

  val a_elevation : float -> [> | `Elevation ] attrib

  val a_pointatx : float -> [> | `PointsAtX ] attrib

  val a_pointaty : float -> [> | `PointsAtY ] attrib

  val a_pointatz : float -> [> | `PointsAtZ ] attrib

  val a_specularexponent : float -> [> | `SpecularExponent ] attrib

  val a_specularconstant : float -> [> | `SpecularConstant ] attrib

  val a_limitingconeangle : float -> [> | `LimitingConeAngle ] attrib

  val a_mode :
    [< | `Normal | `Multiply | `Screen | `Darken | `Lighten ] ->
    [> | `Mode ] attrib

  val a_typefecolor :
    [< | `Matrix | `Saturate | `HueRotate | `LuminanceToAlpha ] ->
    [> | `Typefecolor ] attrib

  val a_values : numbers -> [> | `Values ] attrib

  val a_transferttype :
    [< | `Identity | `Table | `Discrete | `Linear | `Gamma ] ->
    [> | `Typetransfert ] attrib

  val a_tablevalues : numbers -> [> | `TableValues ] attrib

  val a_slope : number -> [> | `Slope ] attrib

  val a_intercept : number -> [> | `Intercept ] attrib

  val a_amplitude : number -> [> | `Amplitude ] attrib

  val a_exponent : number -> [> | `Exponent ] attrib

  val a_offsettransfer : number -> [> | `Offsettransfer ] attrib

  val a_operator :
    [< | `Over | `In | `Out | `Atop | `Xor | `Arithmetic ] ->
    [> | `Operator ] attrib

  val a_k1 : number -> [> | `K1 ] attrib

  val a_k2 : number -> [> | `K2 ] attrib

  val a_k3 : number -> [> | `K3 ] attrib

  val a_k4 : number -> [> | `K4 ] attrib

  val a_order : number_optional_number -> [> | `Order ] attrib

  val a_kernelmatrix : numbers -> [> | `KernelMatrix ] attrib

  val a_divisor : number -> [> | `Divisor ] attrib

  val a_bias : number -> [> | `Bias ] attrib

  val a_kernelunitlength :
    number_optional_number -> [> | `KernelUnitLength ] attrib

  val a_targetX : int -> [> | `TargetX ] attrib

  val a_targetY : int -> [> | `TargetY ] attrib

  val a_edgemode :
    [< | `Duplicate | `Wrap | `None ] -> [> | `TargetY ] attrib

  val a_preservealpha : bool -> [> | `TargetY ] attrib

  val a_surfacescale : number -> [> | `SurfaceScale ] attrib

  val a_diffuseconstant : number -> [> | `DiffuseConstant ] attrib

  val a_scale : number -> [> | `Scale ] attrib

  val a_xchannelselector :
    [< | `R | `G | `B | `A ] -> [> | `XChannelSelector ] attrib

  val a_ychannelselector :
    [< | `R | `G | `B | `A ] -> [> | `YChannelSelector ] attrib

  val a_stddeviation : number_optional_number -> [> | `StdDeviation ] attrib

  val a_operatormorphology :
    [< | `Erode | `Dilate ] -> [> | `OperatorMorphology ] attrib

  val a_radius : number_optional_number -> [> | `Radius ] attrib

  val a_basefrenquency :
    number_optional_number -> [> | `BaseFrequency ] attrib

  val a_numoctaves : int -> [> | `NumOctaves ] attrib

  val a_seed : number -> [> | `Seed ] attrib

  val a_stitchtiles :
    [< | `Stitch | `NoStitch ] -> [> | `StitchTiles ] attrib

  val a_stitchtype :
    [< | `FractalNoise | `Turbulence ] -> [> | `TypeStitch ] attrib

  val a_xlinkshow : [< | `New | `Replace ] -> [> | `Xlink_show ] attrib

  val a_xlinkactuate : [< | `OnRequest ] -> [> | `Xlink_actuate ] attrib

  val a_target : string -> [> | `Xlink_target ] attrib

  val a_viewtarget : string -> [> | `ViewTarget ] attrib

  val a_attributename : string -> [> | `AttributeName ] attrib

  val a_attributetype :
    [< | `CSS | `XML | `Auto ] -> [> | `AttributeType ] attrib

  val a_begin : string -> [> | `Begin ] attrib

  val a_dur : string -> [> | `Dur ] attrib

  (* XXX*)
  val a_min : string -> [> | `Min ] attrib

  (* XXX *)
  val a_max : string -> [> | `Max ] attrib

  (* XXX *)
  val a_restart :
    [< | `Always | `WhenNotActive | `Never ] -> [> | `Restart ] attrib

  val a_repeatcount : string -> [> | `RepeatCount ] attrib

  (* XXX *)
  val a_repeatdur : string -> [> | `RepeatDur ] attrib

  (* XXX *)
  val a_fill : paint -> [> | `Fill ] attrib

  val a_fill_animation : [< | `Freeze | `Remove ] -> [> | `Fill_Animation ] attrib

  val a_calcmode :
    [< | `Discrete | `Linear | `Paced | `Spline ] -> [> | `CalcMode ] attrib

  val a_values_anim : strings -> [> | `Valuesanim ] attrib

  val a_keytimes : strings -> [> | `KeyTimes ] attrib

  val a_keysplines : strings -> [> | `KeySplines ] attrib

  val a_from : string -> [> | `From ] attrib

  val a_to : string -> [> | `To ] attrib

  val a_by : string -> [> | `By ] attrib

  val a_additive : [< | `Replace | `Sum ] -> [> | `Additive ] attrib

  val a_accumulate : [< | `None | `Sum ] -> [> | `Accumulate ] attrib

  val a_keypoints : numbers_semicolon -> [> | `KeyPoints ] attrib

  val a_path : string -> [> | `Path ] attrib

  val a_typeanimatecolor :
    [ | `Translate | `Scale | `Rotate | `SkewX | `SkewY ] ->
    [ | `Typeanimatecolor ] attrib

  val a_horiz_origin_x : number -> [> | `Horizoriginx ] attrib

  val a_horiz_origin_y : number -> [> | `Horizoriginy ] attrib

  val a_horiz_adv_x : number -> [> | `Horizadvx ] attrib

  val a_vert_origin_x : number -> [> | `Vertoriginx ] attrib

  val a_vert_origin_y : number -> [> | `Vertoriginy ] attrib

  val a_vert_adv_y : number -> [> | `Vertadvy ] attrib

  val a_unicode : string -> [> | `Unicode ] attrib

  val a_glyphname : string -> [> | `glyphname ] attrib

  val a_orientation : [< | `H | `V ] -> [> | `Orientation ] attrib

  val a_arabicform :
    [< | `Initial | `Medial | `Terminal | `Isolated ] ->
    [> | `Arabicform ] attrib

  val a_lang : string -> [> | `Lang ] attrib

  val a_u1 : string -> [> | `U1 ] attrib

  val a_u2 : string -> [> | `U2 ] attrib

  val a_g1 : string -> [> | `G1 ] attrib

  val a_g2 : string -> [> | `G2 ] attrib

  val a_k : string -> [> | `K ] attrib

  val a_fontfamily : string -> [> | `FontFamily ] attrib

  val a_fontstyle : string -> [> | `FontStyle ] attrib

  val a_fontvariant : string -> [> | `FontVariant ] attrib

  val a_fontweight : string -> [> | `FontWeight ] attrib

  val a_fontstretch : string -> [> | `FontStretch ] attrib

  val a_fontsize : string -> [> | `FontSize ] attrib

  val a_unicoderange : string -> [> | `UnicodeRange ] attrib

  val a_unitsperem : string -> [> | `UnitsPerEm ] attrib

  val a_stemv : number -> [> | `Stemv ] attrib

  val a_stemh : number -> [> | `Stemh ] attrib

  val a_slope : number -> [> | `Slope ] attrib

  val a_capheight : number -> [> | `CapHeight ] attrib

  val a_xheight : number -> [> | `XHeight ] attrib

  val a_accentheight : number -> [> | `AccentHeight ] attrib

  val a_ascent : number -> [> | `Ascent ] attrib

  val a_widths : string -> [> | `Widths ] attrib

  val a_bbox : string -> [> | `Bbox ] attrib

  val a_ideographic : number -> [> | `Ideographic ] attrib

  val a_alphabetic : number -> [> | `Alphabetic ] attrib

  val a_mathematical : number -> [> | `Mathematical ] attrib

  val a_hanging : number -> [> | `Hanging ] attrib

  val a_videographic : number -> [> | `VIdeographic ] attrib

  val a_valphabetic : number -> [> | `VAlphabetic ] attrib

  val a_vmathematical : number -> [> | `VMathematical ] attrib

  val a_vhanging : number -> [> | `VHanging ] attrib

  val a_underlineposition : number -> [> | `UnderlinePosition ] attrib

  val a_underlinethickness : number -> [> | `UnderlineThickness ] attrib

  val a_strikethroughposition :
    number -> [> | `StrikethroughPosition ] attrib

  val a_strikethroughthickness :
    number -> [> | `StrikethroughThickness ] attrib

  val a_overlineposition : number -> [> | `OverlinePosition ] attrib

  val a_overlinethickness : number -> [> | `OverlineThickness ] attrib

  val a_string : string -> [> | `String ] attrib

  val a_name : string -> [> | `Name ] attrib

  val a_onabort : Xml.event_handler -> [> | `OnAbort ] attrib

  val a_onactivate : Xml.event_handler -> [> | `OnActivate ] attrib

  val a_onbegin : Xml.event_handler -> [> | `OnBegin ] attrib

  val a_onclick : Xml.event_handler -> [> | `OnClick ] attrib

  val a_onend : Xml.event_handler -> [> | `OnEnd ] attrib

  val a_onerror : Xml.event_handler -> [> | `OnError ] attrib

  val a_onfocusin : Xml.event_handler -> [> | `OnFocusIn ] attrib

  val a_onfocusout : Xml.event_handler -> [> | `OnFocusOut ] attrib

  val a_onload : Xml.event_handler -> [> | `OnLoad ] attrib

  val a_onmousedown : Xml.event_handler -> [> | `OnMouseDown ] attrib

  val a_onmouseup : Xml.event_handler -> [> | `OnMouseUp ] attrib

  val a_onmouseover : Xml.event_handler -> [> | `OnMouseOver ] attrib

  val a_onmouseout : Xml.event_handler -> [> | `OnMouseOut ] attrib

  val a_onmousemove : Xml.event_handler -> [> | `OnMouseMove ] attrib

  val a_onrepeat : Xml.event_handler -> [> | `OnRepeat ] attrib

  val a_onresize : Xml.event_handler -> [> | `OnResize ] attrib

  val a_onscroll : Xml.event_handler -> [> | `OnScroll ] attrib

  val a_onunload : Xml.event_handler -> [> | `OnUnload ] attrib

  val a_onzoom : Xml.event_handler -> [> | `OnZoom ] attrib

  val metadata :
    ?a: ((metadata_attr attrib) list) -> Xml.elt list -> [> | metadata] elt

  val foreignobject :
    ?a: ((foreignobject_attr attrib) list) ->
    Xml.elt list -> [> | foreignobject] elt

  val a_stopcolor : color -> [> | `Stop_Color ] attrib

  val a_stopopacity : number -> [> | `Stop_Opacity ] attrib

  val a_stroke : paint -> [> | `Stroke ] attrib

  val a_strokewidth : length -> [> | `Stroke_Width ] attrib

  val a_strokelinecap :
    [< `Butt | `Round | `Square ] -> [> | `Stroke_Linecap ] attrib

  val a_strokelinejoin :
    [< `Miter | `Round | `Bever ] -> [> `Stroke_Linejoin ] attrib

  val a_strokemiterlimit : float -> [> `Stroke_Miterlimit ] attrib

  val a_strokedasharray : Unit.length list -> [> `Stroke_Dasharray ] attrib

  val a_strokedashoffset : Unit.length -> [> `Stroke_Dashoffset ] attrib

  val a_strokeopacity : float -> [> `Stroke_Opacity ] attrib

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
    val data : string -> 'a elt

    (** Insert an XML node that is not implemented in this module.
        If it is a standard SVG node which is missing,
        please report to the Ocsigen team.
    *)
    val node : string -> ?a:'a attrib list -> 'b elt list -> 'c elt

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

  (** {2 ... } *)

  type doc = [ `Svg ] elt
  val doc_toelt : doc -> Xml.elt


end
