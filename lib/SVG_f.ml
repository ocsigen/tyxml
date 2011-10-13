(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2010 by Simon Castellan
 * Copyright (C) 2010 by Cecile Herbelin
 * Copyright (C) 2010 by Vincent Balat
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

(** Type instantiations for SVG *)

(** This module defines basic data types for data, attributes
    and element occuring in SVG documents.
    It is based on the specification available at http://www.w3.org/TR/SVG/.

    This module is experimental, it may lack of some attributes,
    and the interface is very low level and do not take deeply into account
    the needs of SVG elements. *)

open SVG_types

open Unit

let string_of_iri x = x

module Unit = struct

  let rel x     = (x, None)
  let deg x     = (x, Some `Deg)
  let grad x    = (x, Some `Grad)
  let rad x     = (x, Some `Rad)
  let ms x      = (x, Some `Ms)
  let s x       = (x, Some `S)
  let em x      = (x, Some `Em)
  let ex x      = (x, Some `Ex)
  let px x      = (x, Some `Px)
  let in_ x     = (x, Some `In)
  let cm x      = (x, Some `Cm)
  let mm x      = (x, Some `Mm)
  let pt x      = (x, Some `Pt)
  let pc x      = (x, Some `Pc)
  let percent x = (x, Some `Percent)
  let hz x      = (x, Some `Hz)
  let khz x     = (x, Some `KHz)
  let to_string list (n, unit) = Printf.sprintf "%g%s" n begin
    match unit with
    | Some unit -> List.assoc unit list
    | None -> ""
  end
  let deg_names = [ (`Deg, "deg"); (`Grad, "grad"); (`Rad, "rad") ]
  let string_of_angle a = to_string deg_names a
  let time_names = [ (`Ms, "ms"); (`S, "s") ]
  let string_of_time a = to_string time_names a
  let length_names =
    [ (`Em, "em"); (`Ex, "ex"); (`Px, "px"); (`In, "in"); (`Cm, "cm");
      (`Mm, "mm"); (`Pt, "pt"); (`Pc, "pc"); (`Percent, "%") ]
  let string_of_length (a: length) = to_string length_names a

  let freq_names = [ (`Hz, "Hz"); (`KHz, "kHz") ]
  let string_of_freq a = to_string freq_names a

end

open Unit

let string_of_coord = string_of_length
let string_of_number = string_of_float
let string_of_number_optional_number =
  function
  | (x, Some y) -> Printf.sprintf "%g, %g" x y
  | (x, None) -> Printf.sprintf "%g" x
let string_of_percentage = Printf.sprintf "%d%%"
let string_of_strings = String.concat ", "
let string_of_transform =
  function
  | Matrix ((a, b, c, d, e, f)) ->
      Printf.sprintf "matrix(%g %g %g %g %g %g)" a b c d e f
  | Translate x ->
      Printf.sprintf "translate(%s)" (string_of_number_optional_number x)
  | Scale x ->
      Printf.sprintf "scale(%s)" (string_of_number_optional_number x)
  | Rotate ((angle, x)) ->
      Printf.sprintf "rotate(%s %s)" (string_of_angle angle)
        (match x with
         | Some ((x, y)) -> Printf.sprintf "%g %g" x y
         | None -> "")
  | SkewX angle -> Printf.sprintf "skewX(%s)" (string_of_angle angle)
  | SkewY angle -> Printf.sprintf "skewY(%s)" (string_of_angle angle)
let string_of_spacestrings = String.concat " "
let string_of_commastrings = String.concat ", "
let string_of_transforms x = String.concat " " (List.map string_of_transform x)
let string_of_fourfloats (a, b, c, d) = Printf.sprintf "%g %g %g %g" a b c d
let string_of_lengths l = String.concat " " (List.map string_of_length l)
let string_of_numbers l = String.concat " " (List.map string_of_float l)
let string_of_numbers_semicolon l =
  String.concat ";" (List.map string_of_float l)
let string_of_coords l =
  String.concat " " (List.map (fun (a, b) -> Printf.sprintf "%g, %g" a b) l)

module Make(XML : XML_sigs.T) = struct

  module XML = XML

  module Info = struct
    let content_type = "image/svg+xml"
    let alternative_content_types = []
    let emptytags = []
    let version = "SVG 1.1"
    let standard = Uri.uri_of_string "http://www.w3.org/TR/svg11/"
    let namespace = "http://www.w3.org/2000/svg"
    let doctype =
      XML_print.compose_doctype	"svg"
	["-//W3C//DTD SVG 1.1//EN";
	 "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"]
  end

  include Uri
  open Unit

  type 'a attrib = XML.attrib

  type +'a elt = XML.elt

  type +'a elts = XML.elt list

  type ('a, 'b) nullary = ?a: (('a attrib) list) -> unit -> 'b elt

  type ('a, 'b, 'c) unary = ?a: (('a attrib) list) -> 'b elt -> 'c elt

  type ('a, 'b, 'c) star =
      ?a: (('a attrib) list) -> ('b elt) list -> 'c elt

  type ('a, 'b, 'c) plus =
      ?a: (('a attrib) list) -> 'b elt -> ('b elt) list -> 'c elt

  let tot x = x

  let totl x = x

  let toelt x = x

  let toeltl x = x

  let to_xmlattribs x = x
  let to_attrib x = x

  let nullary tag ?a () = XML.node ?a tag []

  let unary tag ?a elt = XML.node ?a tag [ elt ]

  let star tag ?a elts = XML.node ?a tag elts

  let plus tag ?a elt elts = XML.node ?a tag (elt :: elts)

  type altglyphdef_content =
      [ | `Ref of (glyphref elt) list | `Item of (altglyphitem elt) list
      ]

  let string_of_string s = s

  let to_xmlattribs x = x

  let float_attrib = XML.float_attrib

  let int_attrib = XML.int_attrib

  let string_attrib = XML.string_attrib

  let uri_attrib a s = XML.string_attrib a (string_of_uri s)

  let user_attrib f name v = XML.string_attrib name (f v)

  let metadata ?a children = XML.node ?a "metadata" children

  let foreignobject ?a children = XML.node ?a "foreignObject" children

  (* generated *)
  let a_version = user_attrib string_of_string "version"

  let a_baseprofile = user_attrib string_of_string "baseProfile"

  let a_x = user_attrib string_of_coord "x"

  let a_y = user_attrib string_of_coord "y"

  let a_width = user_attrib string_of_length "width"

  let a_height = user_attrib string_of_length "height"

  let a_preserveaspectratio =
    user_attrib string_of_string "preserveAspectRatio"

  let a_contentscripttype =
    user_attrib string_of_string "contentScriptType"

  let a_contentstyletype = user_attrib string_of_string "contentStyleType"

  let a_zoomAndPan =
    user_attrib (function | `Disable -> "disable" | `Magnify -> "magnify")
      "zoomAndSpan"

  let a_xlink_href = user_attrib string_of_iri "xlink:href"

  let a_requiredfeatures =
    user_attrib string_of_spacestrings "requiredFeatures"

  let a_requiredextensions =
    user_attrib string_of_spacestrings "requiredExtension"

  let a_systemlanguage =
    user_attrib string_of_commastrings "systemLanguage"

  let a_externalressourcesrequired =
    user_attrib string_of_bool "externalRessourcesRequired"

  let a_id = user_attrib string_of_string "id"

  let a_xml_base = user_attrib string_of_iri "xml:base"

  let a_xml_lang = user_attrib string_of_iri "xml:lang"

  let a_xml_space =
    user_attrib
      (function | `Default -> "default" | `Preserve -> "preserve")
      "xml:space"

  let a_type = user_attrib string_of_string "type"

  let a_media = user_attrib string_of_commastrings "media"

  let a_title = user_attrib string_of_string "title"

  let a_class = user_attrib string_of_spacestrings "class"

  let a_style = user_attrib string_of_string "style"

  let a_transform = user_attrib string_of_transform "transform"

  let a_viewbox = user_attrib string_of_fourfloats "viewbox"

  let a_d = user_attrib string_of_string "d"

  let a_pathlength = user_attrib string_of_float "pathLength"

  let a_rx = user_attrib string_of_length "rx"

  let a_ry = user_attrib string_of_length "ry"

  let a_cx = user_attrib string_of_length "cx"

  let a_cy = user_attrib string_of_length "cy"

  let a_r = user_attrib string_of_length "r"

  let a_x1 = user_attrib string_of_coord "x1"

  let a_y1 = user_attrib string_of_coord "y1"

  let a_x2 = user_attrib string_of_coord "x2"

  let a_y2 = user_attrib string_of_coord "y2"

  let a_points = user_attrib string_of_coords "points"

  let a_x_list = user_attrib string_of_lengths "x"

  let a_y_list = user_attrib string_of_lengths "y"

  let a_dx = user_attrib string_of_lengths "dx"

  let a_dy = user_attrib string_of_lengths "dy"

  let a_dx_single = user_attrib string_of_length "dx"

  let a_dy_single = user_attrib string_of_length "dy"

  let a_dx_number = user_attrib string_of_number "dx"

  let a_dy_number = user_attrib string_of_number "dy"

  let a_lengthadjust =
    user_attrib
      (function
        | `Spacing -> "spacing"
        | `SpacingAndGlyphs -> "spacingAndGlyphs")
      "lengthAdjust"

  let a_textlength = user_attrib string_of_length "textLength"

  let a_rotate = user_attrib string_of_numbers "rotate"

  let a_startoffset = user_attrib string_of_length "startOffset"

  let a_method =
    user_attrib (function | `Align -> "align" | `Stretch -> "stretch")
      "method"

  let a_spacing =
    user_attrib (function | `Auto -> "auto" | `Exact -> "exact") "spacing"

  let a_glyphref = user_attrib string_of_string "glyphRef"

  let a_format = user_attrib string_of_string "format"

  let a_markerunits =
    user_attrib
      (function
        | `StrokeWidth -> "strokeWidth"
        | `UserSpaceOnUse -> "userSpaceOnUse")
      "markerUnits"

  let a_refx = user_attrib string_of_coord "refX"

  let a_refy = user_attrib string_of_coord "refY"

  let a_markerwidth = user_attrib string_of_length "markerWidth"

  let a_markerheight = user_attrib string_of_length "markerHeight"

  let a_orient =
    user_attrib
      (function | `Auto -> "auto" | `Angle __svg -> string_of_angle __svg)
      "orient"

  let a_local = user_attrib string_of_string "local"

  let a_string = user_attrib string_of_string "name"

  let a_renderingindent =
    user_attrib
      (function
        | `Auto -> "auto"
        | `Perceptual -> "perceptual"
        | `Relative_colorimetric -> "relative_colorimetric"
        | `Saturation -> "saturation"
        | `Absolute_colorimetric -> "absolute_colorimetric")
      "rendering:indent"

  let a_gradientunits =
    user_attrib
      (function
        | `UserSpaceOnUse -> "userSpaceOnUse"
        | `ObjectBoundingBox -> "objectBoundingBox")
      "gradientUnits"

  let a_gradienttransform =
    user_attrib string_of_transforms "gradient:transform"

  let a_spreadmethod =
    user_attrib
      (function
        | `Pad -> "pad"
        | `Reflect -> "reflect"
        | `Repeat -> "repeat")
      "spreadMethod"

  let a_fx = user_attrib string_of_coord "fx"

  let a_fy = user_attrib string_of_coord "fy"

  let a_offset =
    user_attrib
      (function
        | `Number __svg -> string_of_number __svg
        | `Percentage __svg -> string_of_percentage __svg)
      "offset"

  let a_patternunits =
    user_attrib
      (function
        | `UserSpaceOnUse -> "userSpaceOnUse"
        | `ObjectBoundingBox -> "objectBoundingBox")
      "patternUnits"

  let a_patterncontentunits =
    user_attrib
      (function
        | `UserSpaceOnUse -> "userSpaceOnUse"
        | `ObjectBoundingBox -> "objectBoundingBox")
      "patternContentUnits"

  let a_patterntransform =
    user_attrib string_of_transforms "patternTransform"

  let a_clippathunits =
    user_attrib
      (function
        | `UserSpaceOnUse -> "userSpaceOnUse"
        | `ObjectBoundingBox -> "objectBoundingBox")
      "clipPathUnits"

  let a_maskunits =
    user_attrib
      (function
        | `UserSpaceOnUse -> "userSpaceOnUse"
        | `ObjectBoundingBox -> "objectBoundingBox")
      "maskUnits"

  let a_maskcontentunits =
    user_attrib
      (function
        | `UserSpaceOnUse -> "userSpaceOnUse"
        | `ObjectBoundingBox -> "objectBoundingBox")
      "maskContentUnits"

  let a_primitiveunits =
    user_attrib
      (function
        | `UserSpaceOnUse -> "userSpaceOnUse"
        | `ObjectBoundingBox -> "objectBoundingBox")
      "primitiveUnits"

  let a_filterres =
    user_attrib string_of_number_optional_number "filterResUnits"

  let a_result = user_attrib string_of_string "result"

  let a_in =
    user_attrib
      (function
        | `SourceGraphic -> "sourceGraphic"
        | `SourceAlpha -> "sourceAlpha"
        | `BackgroundImage -> "backgroundImage"
        | `BackgroundAlpha -> "backgroundAlpha"
        | `FillPaint -> "fillPaint"
        | `StrokePaint -> "strokePaint"
        | `Ref __svg -> string_of_string __svg)
      "in"

  let a_in2 =
    user_attrib
      (function
        | `SourceGraphic -> "sourceGraphic"
        | `SourceAlpha -> "sourceAlpha"
        | `BackgroundImage -> "backgroundImage"
        | `BackgroundAlpha -> "backgroundAlpha"
        | `FillPaint -> "fillPaint"
        | `StrokePaint -> "strokePaint"
        | `Ref __svg -> string_of_string __svg)
      "in2"

  let a_aizmuth = user_attrib string_of_float "azimuth"

  let a_elevation = user_attrib string_of_float "elevation"

  let a_pointatx = user_attrib string_of_float "pointsAtX"

  let a_pointaty = user_attrib string_of_float "pointsAtY"

  let a_pointatz = user_attrib string_of_float "pointsAtZ"

  let a_specularexponent = user_attrib string_of_float "specularExponent"

  let a_specularconstant = user_attrib string_of_float "specularConstant"

  let a_limitingconeangle = user_attrib string_of_float "limitingConeAngle"

  let a_mode =
    user_attrib
      (function
        | `Normal -> "normal"
        | `Multiply -> "multiply"
        | `Screen -> "screen"
        | `Darken -> "darken"
        | `Lighten -> "lighten")
      "mode"

  let a_typefecolor =
    user_attrib
      (function
        | `Matrix -> "matrix"
        | `Saturate -> "saturate"
        | `HueRotate -> "hueRotate"
        | `LuminanceToAlpha -> "luminanceToAlpha")
      "type"

  let a_values = user_attrib string_of_numbers "values"

  let a_transferttype =
    user_attrib
      (function
        | `Identity -> "identity"
        | `Table -> "table"
        | `Discrete -> "discrete"
        | `Linear -> "linear"
        | `Gamma -> "gamma")
      "type"

  let a_tablevalues = user_attrib string_of_numbers "tableValues"

  let a_slope = user_attrib string_of_number "slope"

  let a_intercept = user_attrib string_of_number "intercept"

  let a_amplitude = user_attrib string_of_number "amplitude"

  let a_exponent = user_attrib string_of_number "exponent"

  let a_offsettransfer = user_attrib string_of_number "offset"

  let a_operator =
    user_attrib
      (function
        | `Over -> "over"
        | `In -> "in"
        | `Out -> "out"
        | `Atop -> "atop"
        | `Xor -> "xor"
        | `Arithmetic -> "arithmetic")
      "operator"

  let a_k1 = user_attrib string_of_number "k1"

  let a_k2 = user_attrib string_of_number "k2"

  let a_k3 = user_attrib string_of_number "k3"

  let a_k4 = user_attrib string_of_number "k4"

  let a_order = user_attrib string_of_number_optional_number "order"

  let a_kernelmatrix = user_attrib string_of_numbers "kernelMatrix"

  let a_divisor = user_attrib string_of_number "divisor"

  let a_bias = user_attrib string_of_number "bias"

  let a_kernelunitlength =
    user_attrib string_of_number_optional_number "kernelUnitLength"

  let a_targetX = user_attrib string_of_int "targetX"

  let a_targetY = user_attrib string_of_int "targetY"

  let a_edgemode =
    user_attrib
      (function
        | `Duplicate -> "duplicate"
        | `Wrap -> "wrap"
        | `None -> "none")
      "targetY"

  let a_preservealpha = user_attrib string_of_bool "targetY"

  let a_surfacescale = user_attrib string_of_number "surfaceScale"

  let a_diffuseconstant = user_attrib string_of_number "diffuseConstant"

  let a_scale = user_attrib string_of_number "scale"

  let a_xchannelselector =
    user_attrib (function | `R -> "r" | `G -> "g" | `B -> "b" | `A -> "a")
      "xChannelSelector"

  let a_ychannelselector =
    user_attrib (function | `R -> "r" | `G -> "g" | `B -> "b" | `A -> "a")
      "yChannelSelector"

  let a_stddeviation =
    user_attrib string_of_number_optional_number "stdDeviation"

  let a_operatormorphology =
    user_attrib (function | `Erode -> "erode" | `Dilate -> "dilate")
      "operatorMorphology"

  let a_radius = user_attrib string_of_number_optional_number "radius"

  let a_basefrenquency =
    user_attrib string_of_number_optional_number "baseFrequency"

  let a_numoctaves = user_attrib string_of_int "numOctaves"

  let a_seed = user_attrib string_of_number "seed"

  let a_stitchtiles =
    user_attrib (function | `Stitch -> "stitch" | `NoStitch -> "noStitch")
      "stitchTiles"

  let a_stitchtype =
    user_attrib
      (function
        | `FractalNoise -> "fractalNoise"
        | `Turbulence -> "turbulence")
      "typeStitch"

  let a_xlinkshow =
    user_attrib (function | `New -> "new" | `Replace -> "replace")
      "xlink:show"

  let a_xlinkactuate =
    user_attrib (function | `OnRequest -> "onRequest") "xlink:actuate"

  let a_target = user_attrib string_of_string "xlink:target"

  let a_viewtarget = user_attrib string_of_string "viewTarget"

  let a_attributename = user_attrib string_of_string "attributeName"

  let a_attributetype =
    user_attrib
      (function | `CSS -> "cSS" | `XML -> "xML" | `Auto -> "auto")
      "attributeType"

  let a_begin = user_attrib string_of_string "begin"

  let a_dur = user_attrib string_of_string "dur"

  let a_min = user_attrib string_of_string "min"

  let a_max = user_attrib string_of_string "max"

  let a_restart =
    user_attrib
      (function
        | `Always -> "always"
        | `WhenNotActive -> "whenNotActive"
        | `Never -> "never")
      "restart"

  let a_repeatcount = user_attrib string_of_string "repeatCount"

  let a_repeatdur = user_attrib string_of_string "repeatDur"

  let a_fill =
    user_attrib (function | `Freeze -> "freeze" | `Remove -> "remove")
      "fill"

  let a_calcmode =
    user_attrib
      (function
        | `Discrete -> "discrete"
        | `Linear -> "linear"
        | `Paced -> "paced"
        | `Spline -> "spline")
      "calcMode"

  let a_values_anim = user_attrib string_of_strings "values"

  let a_keytimes = user_attrib string_of_strings "keyTimes"

  let a_keysplines = user_attrib string_of_strings "keySplines"

  let a_from = user_attrib string_of_string "from"

  let a_to = user_attrib string_of_string "to"

  let a_by = user_attrib string_of_string "by"

  let a_additive =
    user_attrib (function | `Replace -> "replace" | `Sum -> "sum")
      "additive"

  let a_accumulate =
    user_attrib (function | `None -> "none" | `Sum -> "sum") "accumulate"

  let a_keypoints = user_attrib string_of_numbers_semicolon "keyPoints"

  let a_path = user_attrib string_of_string "path"

  let a_typeanimatecolor =
    user_attrib
      (function
        | `Translate -> "translate"
        | `Scale -> "scale"
        | `Rotate -> "rotate"
        | `SkewX -> "skewX"
        | `SkewY -> "skewY")
      "type"

  let a_horiz_origin_x = user_attrib string_of_number "horiz-origin-x"

  let a_horiz_origin_y = user_attrib string_of_number "horiz-origin-y"

  let a_horiz_adv_x = user_attrib string_of_number "horiz-adv-x"

  let a_vert_origin_x = user_attrib string_of_number "vert-origin-x"

  let a_vert_origin_y = user_attrib string_of_number "vert-origin-y"

  let a_vert_adv_y = user_attrib string_of_number "vert-adv-y"

  let a_unicode = user_attrib string_of_string "unicode"

  let a_glyphname = user_attrib string_of_string "glyphname"

  let a_orientation =
    user_attrib (function | `H -> "h" | `V -> "v") "orientation"

  let a_arabicform =
    user_attrib
      (function
        | `Initial -> "initial"
        | `Medial -> "medial"
        | `Terminal -> "terminal"
        | `Isolated -> "isolated")
      "arabic-form"

  let a_lang = user_attrib string_of_string "lang"

  let a_u1 = user_attrib string_of_string "u1"

  let a_u2 = user_attrib string_of_string "u2"

  let a_g1 = user_attrib string_of_string "g1"

  let a_g2 = user_attrib string_of_string "g2"

  let a_k = user_attrib string_of_string "k"

  let a_fontfamily = user_attrib string_of_string "font-family"

  let a_fontstyle = user_attrib string_of_string "font-style"

  let a_fontvariant = user_attrib string_of_string "font-variant"

  let a_fontweight = user_attrib string_of_string "font-weight"

  let a_fontstretch = user_attrib string_of_string "font-stretch"

  let a_fontsize = user_attrib string_of_string "font-size"

  let a_unicoderange = user_attrib string_of_string "unicode-range"

  let a_unitsperem = user_attrib string_of_string "units-per-em"

  let a_stemv = user_attrib string_of_number "stemv"

  let a_stemh = user_attrib string_of_number "stemh"

  let a_slope = user_attrib string_of_number "slope"

  let a_capheight = user_attrib string_of_number "cap-height"

  let a_xheight = user_attrib string_of_number "x-height"

  let a_accentheight = user_attrib string_of_number "accent-height"

  let a_ascent = user_attrib string_of_number "ascent"

  let a_widths = user_attrib string_of_string "widths"

  let a_bbox = user_attrib string_of_string "bbox"

  let a_ideographic = user_attrib string_of_number "ideographic"

  let a_alphabetic = user_attrib string_of_number "alphabetic"

  let a_mathematical = user_attrib string_of_number "mathematical"

  let a_hanging = user_attrib string_of_number "hanging"

  let a_videographic = user_attrib string_of_number "v-ideographic"

  let a_valphabetic = user_attrib string_of_number "v-alphabetic"

  let a_vmathematical = user_attrib string_of_number "v-mathematical"

  let a_vhanging = user_attrib string_of_number "v-hanging"

  let a_underlineposition =
    user_attrib string_of_number "underline-position"

  let a_underlinethickness =
    user_attrib string_of_number "underline-thickness"

  let a_strikethroughposition =
    user_attrib string_of_number "strikethrough-position"

  let a_strikethroughthickness =
    user_attrib string_of_number "strikethrough-thickness"

  let a_overlineposition = user_attrib string_of_number "overline-position"

  let a_overlinethickness =
    user_attrib string_of_number "overline-thickness"

  let a_string = user_attrib string_of_string "string"

  let a_name = user_attrib string_of_string "name"

  let a_onabort = user_attrib string_of_string "onabort"

  let a_onactivate = user_attrib string_of_string "onactivate"

  let a_onbegin = user_attrib string_of_string "onbegin"

  let a_onclick = user_attrib string_of_string "onclick"

  let a_onend = user_attrib string_of_string "onend"

  let a_onerror = user_attrib string_of_string "onerror"

  let a_onfocusin = user_attrib string_of_string "onfocusin"

  let a_onfocusout = user_attrib string_of_string "onfocusout"

  let a_onload = user_attrib string_of_string "onload"

  let a_onmousedown = user_attrib string_of_string "onmousdown"

  let a_onmouseup = user_attrib string_of_string "onmouseup"

  let a_onmouseover = user_attrib string_of_string "onmouseover"

  let a_onmouseout = user_attrib string_of_string "onmouseout"

  let a_onmousemove = user_attrib string_of_string "onmousemove"

  let a_onrepeat = user_attrib string_of_string "onrepeat"

  let a_onresize = user_attrib string_of_string "onresize"

  let a_onscroll = user_attrib string_of_string "onscroll"

  let a_onunload = user_attrib string_of_string "onunload"

  let a_onzoom = user_attrib string_of_string "onzoom"

  (* also generated *)
  let svg = star "svg"

  let g = star "g"

  let defs = star "defs"

  let desc = unary "desc"

  let title = unary "title"

  let symbol = star "symbol"

  let use = star "use"

  let image = star "image"

  let switch = star "switch"

  let style = unary "style"

  let path = star "path"

  let rect = star "rect"

  let circle = star "circle"

  let ellipse = star "ellipse"

  let line = star "line"

  let polyline = star "polyline"

  let polygon = star "polygon"

  let text = star "text"

  let tspan = star "tspan"

  let tref = star "tref"

  let textpath = star "textPath"

  let altglyph = unary "altGlyph"

  let altglyphdef = unary "altGlyphDef"

  let altglyphitem = plus "altGlyphItem"

  let glyphref = nullary "glyphRef];"

  let marker = star "marker"

  let colorprofile = star "colorProfile"

  let lineargradient = star "linear-gradient"

  let radialgradient = star "radial-gradient"

  let gradientstop = star "gradient-stop"

  let pattern = star "pattern"

  let clippath = star "clipPath"

  let filter = star "filter"

  let fedistantlight = star "feDistantLight"

  let fepointlight = star "fePointLight"

  let fespotlight = star "feSpotLight"

  let feblend = star "feBlend"

  let fecolormatrix = star "feColorMatrix"

  let fecomponenttransfer = star "feComponentTransfer"

  let fefunca = star "feFuncA"

  let fefuncg = star "feFuncA"

  let fefuncb = star "feFuncA"

  let fefuncr = star "feFuncA"

  let fecomposite = star "(*"

  let feconvolvematrix = star "feConvolveMatrix"

  let fediffuselighting = star "(*"

  let fedisplacementmap = star "feDisplacementMap];"

  let feflood = star "(*"

  let fegaussianblur = star "];"

  let feimage = star "(*"

  let femerge = star "feMerge"

  let femorphology = star "feMorphology"

  let feoffset = star "feOffset"

  let fespecularlighting = star "feSpecularLighting"

  let fetile = star "feTile"

  let feturbulence = star "feTurbulence"

  let cursor = star "(*"

  let a = star "a"

  let view = star "view"

  let script = unary "script"

  let animation = star "(*"

  let set = star "set"

  let animatemotion = star "animateMotion"

  let mpath = star "mpath"

  let animatecolor = star "animateColor"

  let animatetransform = star "animateTransform"

  let font = star "font"

  let glyph = star "glyph"

  let missingglyph = star "missingGlyph"

  let hkern = nullary "hkern"

  let vkern = nullary "vkern"

  let fontface = nullary "fontFace"

  let fontfacesrc = star "font-face-src"

  let fontfaceuri = star "font-face-uri"

  let fontfaceformat = nullary "font-face-uri"

  let fontfacename = nullary "font-face-name"

  type doc = [ `Svg ] elt
  let doc_toelt x = x

end
