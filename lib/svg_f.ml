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

open Svg_types

open Unit

let string_of_iri x = Printf.sprintf "url(%s)" x

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

  let to_string f (n, unit) = Printf.sprintf "%g%s" n begin
    match unit with
    | Some unit -> f unit
    | None -> ""
    end

  let angle_names = function `Deg -> "deg" | `Grad -> "grad" | `Rad -> "rad"
  let string_of_angle a = to_string angle_names a

  let time_names = function `Ms -> "ms" | `S -> "s"
  let string_of_time a = to_string time_names a

  let length_names = function
    | `Em -> "em" | `Ex -> "ex" | `Px -> "px" | `In -> "in" | `Cm -> "cm"
    | `Mm -> "mm" | `Pt -> "pt" | `Pc -> "pc" | `Percent -> "%"
  let string_of_length (a: length) = to_string length_names a

  let freq_names = function `Hz -> "Hz" | `KHz -> "kHz"
  let string_of_freq a = to_string freq_names a

end

open Unit

let opt_concat ?(sep=" ") s f = function
  | Some x -> s ^ sep ^ (f x)
  | None -> s

let list ?(sep=" ") f l = String.concat sep (List.map f l)
let comma_list = list ~sep:", "

let string_of_coord = string_of_length
let string_of_number = string_of_float
let string_of_number_optional_number =
  function
  | (x, Some y) -> Printf.sprintf "%g, %g" x y
  | (x, None) -> Printf.sprintf "%g" x
let string_of_percentage = Printf.sprintf "%d%%"


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
let string_of_transforms x = String.concat " " (List.map string_of_transform x)
let string_of_fourfloats (a, b, c, d) = Printf.sprintf "%g %g %g %g" a b c d

let string_of_lengths = list string_of_length
let string_of_numbers = list string_of_float
let string_of_numbers_semicolon = list ~sep:"; " string_of_float

let string_of_coords = list (fun (a, b) -> Printf.sprintf "%g, %g" a b)

let string_of_color s = s
(* For now just string, we may want something better in the future. *)

let string_of_icccolor s = s

let string_of_paint_whitout_icc = function
  | `None -> "none"
  | `CurrentColor -> "currentColor"
  | `Color (c, icc) -> opt_concat (string_of_color c) string_of_icccolor icc

let string_of_paint = function
  | `Icc (iri, None) -> string_of_iri iri
  | `Icc (iri, Some b) ->
      (string_of_iri iri) ^" "^ (string_of_paint_whitout_icc b)
  | #paint_whitout_icc as c -> string_of_paint_whitout_icc c

module MakeWrapped
    (W : Xml_wrap.T)
    (Xml : Xml_sigs.Wrapped with type 'a wrap = 'a W.t) =
struct

  module Xml = Xml

  module Info = struct
    let content_type = "image/svg+xml"
    let alternative_content_types = []
    let emptytags = []
    let version = "SVG 1.1"
    let standard = "http://www.w3.org/TR/svg11/"
    let namespace = "http://www.w3.org/2000/svg"
    let doctype =
      Xml_print.compose_doctype	"svg"
	["-//W3C//DTD SVG 1.1//EN";
	 "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"]
  end

  open Unit

  type uri = Xml.uri
  let string_of_uri = Xml.string_of_uri
  let uri_of_string = Xml.uri_of_string


  (* Mandatory XML stuff. *)

  type 'a attrib = Xml.attrib

  type +'a elt = Xml.elt

  type 'a wrap = 'a W.t

  type +'a elts = Xml.elt list

  type ('a, 'b) nullary = ?a: (('a attrib) list) -> unit -> 'b elt

  type ('a, 'b, 'c) unary = ?a: (('a attrib) list) -> 'b elt wrap -> 'c elt

  type ('a, 'b, 'c) star =
      ?a: (('a attrib) list) -> ('b elt) list wrap -> 'c elt

  type ('a, 'b, 'c) plus =
      ?a: (('a attrib) list) -> 'b elt wrap -> ('b elt) list wrap -> 'c elt

  let tot x = x

  let totl x = x

  let toelt x = x

  let toeltl x = x

  let string_of_string x = x

  let to_xmlattribs x = x
  let to_attrib x = x

  let nullary tag ?a () =
    Xml.node ?a tag (W.return [])

  let unary tag ?a elt =
    Xml.node ?a tag (W.fmap (fun x -> [ x ]) elt)

  let star tag ?a elts = Xml.node ?a tag elts

  let plus tag ?a elt elts =
    let l = W.fmap2 (fun x y -> x :: y) elt elts in
    Xml.node ?a tag l

  type altglyphdef_content =
    [ `Ref of (glyphref elt) list
    | `Item of (altglyphitem elt) list
    ]

  let to_xmlattribs x = x

  let float_attrib = Xml.float_attrib

  let int_attrib = Xml.int_attrib

  let string_attrib = Xml.string_attrib

  let uri_attrib = Xml.uri_attrib


  (* Custom XML attributes *)

  let user_attrib f name v =
    Xml.string_attrib name (W.fmap f v)

  let number_attrib = float_attrib


  (* SVG attributes *)

  let metadata ?a children = Xml.node ?a "metadata" children

  let foreignobject ?a children = Xml.node ?a "foreignObject" children

  let pcdata s = Xml.pcdata s

  (* generated *)
  let a_version = string_attrib "version"

  let a_baseprofile = string_attrib "baseProfile"

  let a_x = user_attrib string_of_coord "x"

  let a_y = user_attrib string_of_coord "y"

  let a_width = user_attrib string_of_length "width"

  let a_height = user_attrib string_of_length "height"

  let a_preserveaspectratio =
    string_attrib "preserveAspectRatio"

  let a_contentscripttype =
    string_attrib "contentScriptType"

  let a_contentstyletype = string_attrib "contentStyleType"

  let a_zoomAndPan x =
    let f = function `Disable -> "disable" | `Magnify -> "magnify" in
    user_attrib f "zoomAndSpan" x

  let a_xlink_href = user_attrib string_of_iri "xlink:href"

  let a_requiredfeatures =
    Xml.space_sep_attrib "requiredFeatures"

  let a_requiredextensions =
    Xml.space_sep_attrib "requiredExtension"

  let a_systemlanguage =
    Xml.comma_sep_attrib "systemLanguage"

  let a_externalressourcesrequired =
    user_attrib string_of_bool "externalRessourcesRequired"

  let a_id = string_attrib "id"

  let a_xml_base = user_attrib string_of_iri "xml:base"

  let a_xml_lang = user_attrib string_of_iri "xml:lang"

  let a_xml_space x =
    let f = function `Default -> "default" | `Preserve -> "preserve" in
    user_attrib f "xml:space" x

  let a_type = string_attrib "type"

  let a_media = Xml.comma_sep_attrib "media"

  let a_title = string_attrib "title"

  let a_class = Xml.space_sep_attrib "class"

  let a_style = string_attrib "style"

  let a_transform = user_attrib string_of_transform "transform"

  let a_viewbox = user_attrib string_of_fourfloats "viewBox"

  let a_d = string_attrib "d"

  let a_pathlength = number_attrib "pathLength"

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

  let a_dx = user_attrib string_of_number "dx"

  let a_dy = user_attrib string_of_number "dy"

  let a_dx_list = user_attrib string_of_lengths "dx"

  let a_dy_list = user_attrib string_of_lengths "dy"

  let a_lengthadjust x =
    let f = function
      | `Spacing -> "spacing"
      | `SpacingAndGlyphs -> "spacingAndGlyphs" in
    user_attrib f "lengthAdjust" x

  let a_textlength = user_attrib string_of_length "textLength"

  let a_rotate = user_attrib string_of_numbers "rotate"

  let a_startoffset = user_attrib string_of_length "startOffset"

  let a_method x =
    let f = function | `Align -> "align" | `Stretch -> "stretch" in
    user_attrib f "method" x

  let a_spacing x =
    let f = function | `Auto -> "auto" | `Exact -> "exact" in
    user_attrib f "spacing" x

  let a_glyphref = string_attrib "glyphRef"

  let a_format = string_attrib "format"

  let a_markerunits x =
    let f = function `StrokeWidth -> "strokeWidth"| `UserSpaceOnUse -> "userSpaceOnUse" in
    user_attrib f "markerUnits" x

  let a_refx = user_attrib string_of_coord "refX"

  let a_refy = user_attrib string_of_coord "refY"

  let a_markerwidth = user_attrib string_of_length "markerWidth"

  let a_markerheight = user_attrib string_of_length "markerHeight"

  let a_orient x =
    let f = function | `Auto -> "auto" | `Angle __svg -> string_of_angle __svg in
    user_attrib f "orient" x

  let a_local = string_attrib "local"

  let a_string = string_attrib "name"

  let a_renderingindent x =
    let f = function
      | `Auto -> "auto" | `Perceptual -> "perceptual"
      | `Relative_colorimetric -> "relative_colorimetric" | `Saturation -> "saturation"
      | `Absolute_colorimetric -> "absolute_colorimetric" in
    user_attrib f "rendering:indent" x

  let a_gradientunits x =
    let f = function
      | `UserSpaceOnUse -> "userSpaceOnUse"
      | `ObjectBoundingBox -> "objectBoundingBox" in
    user_attrib f "gradientUnits" x

  let a_gradienttransform =
    user_attrib string_of_transforms "gradient:transform"

  let a_spreadmethod x =
    let f = function
      | `Pad -> "pad"| `Reflect -> "reflect"| `Repeat -> "repeat" in
    user_attrib f "spreadMethod" x

  let a_fx = user_attrib string_of_coord "fx"

  let a_fy = user_attrib string_of_coord "fy"

  let a_offset x =
    let f = function
      | `Number x -> string_of_number x
      | `Percentage x -> string_of_percentage x in
    user_attrib f "offset" x

  let a_patternunits x =
    let f = function
      | `UserSpaceOnUse -> "userSpaceOnUse"| `ObjectBoundingBox -> "objectBoundingBox" in
    user_attrib f "patternUnits" x

  let a_patterncontentunits x =
    let f = function | `UserSpaceOnUse -> "userSpaceOnUse"| `ObjectBoundingBox -> "objectBoundingBox" in
    user_attrib f "patternContentUnits" x

  let a_patterntransform x =
    user_attrib string_of_transforms "patternTransform" x

  let a_clippathunits x =
    let f = function
      | `UserSpaceOnUse -> "userSpaceOnUse"
      | `ObjectBoundingBox -> "objectBoundingBox" in
    user_attrib f "clipPathUnits" x

  let a_maskunits x =
    let f = function
      | `UserSpaceOnUse -> "userSpaceOnUse"
      | `ObjectBoundingBox -> "objectBoundingBox" in
    user_attrib f "maskUnits" x

  let a_maskcontentunits x =
    let f = function
      | `UserSpaceOnUse -> "userSpaceOnUse"
      | `ObjectBoundingBox -> "objectBoundingBox" in
    user_attrib f "maskContentUnits" x

  let a_primitiveunits x =
    let f = function
      | `UserSpaceOnUse -> "userSpaceOnUse"
      | `ObjectBoundingBox -> "objectBoundingBox" in
    user_attrib f "primitiveUnits" x

  let a_filterres x =
    user_attrib string_of_number_optional_number "filterResUnits" x

  let a_result = string_attrib "result"

  let a_in x =
    let f = function
      | `SourceGraphic -> "sourceGraphic"
      | `SourceAlpha -> "sourceAlpha"
      | `BackgroundImage -> "backgroundImage"
      | `BackgroundAlpha -> "backgroundAlpha"
      | `FillPaint -> "fillPaint"
      | `StrokePaint -> "strokePaint"
      | `Ref _svg -> _svg in
    user_attrib f "in" x

  let a_in2 x =
    let f = function
      | `SourceGraphic -> "sourceGraphic"
      | `SourceAlpha -> "sourceAlpha"
      | `BackgroundImage -> "backgroundImage"
      | `BackgroundAlpha -> "backgroundAlpha"
      | `FillPaint -> "fillPaint"
      | `StrokePaint -> "strokePaint"
      | `Ref _svg -> _svg in
    user_attrib f "in2" x

  let a_aizmuth = number_attrib "azimuth"

  let a_elevation = number_attrib "elevation"

  let a_pointatx = number_attrib "pointsAtX"

  let a_pointaty = number_attrib "pointsAtY"

  let a_pointatz = number_attrib "pointsAtZ"

  let a_specularexponent = number_attrib "specularExponent"

  let a_specularconstant = number_attrib "specularConstant"

  let a_limitingconeangle = number_attrib "limitingConeAngle"

  let a_mode x =
    let f = function
      | `Normal -> "normal"
      | `Multiply -> "multiply"
      | `Screen -> "screen"
      | `Darken -> "darken"
      | `Lighten -> "lighten" in
    user_attrib f "mode" x

  let a_typefecolor x =
    let f = function
      | `Matrix -> "matrix"
      | `Saturate -> "saturate"
      | `HueRotate -> "hueRotate"
      | `LuminanceToAlpha -> "luminanceToAlpha" in
    user_attrib f "type" x

  let a_values = user_attrib string_of_numbers "values"

  let a_transferttype x =
    let f = function
      | `Identity -> "identity"
      | `Table -> "table"
      | `Discrete -> "discrete"
      | `Linear -> "linear"
      | `Gamma -> "gamma" in
    user_attrib f "type" x

  let a_tablevalues = user_attrib string_of_numbers "tableValues"

  let a_slope = user_attrib string_of_number "slope"

  let a_intercept = user_attrib string_of_number "intercept"

  let a_amplitude = user_attrib string_of_number "amplitude"

  let a_exponent = user_attrib string_of_number "exponent"

  let a_offsettransfer = user_attrib string_of_number "offset"

  let a_operator x =
    let f = function
      | `Over -> "over"
      | `In -> "in"
      | `Out -> "out"
      | `Atop -> "atop"
      | `Xor -> "xor"
      | `Arithmetic -> "arithmetic" in
    user_attrib f "operator" x

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

  let a_edgemode x =
    let f = function
      | `Duplicate -> "duplicate"
      | `Wrap -> "wrap"
      | `None -> "none" in
    user_attrib f "targetY" x

  let a_preservealpha = user_attrib string_of_bool "targetY"

  let a_surfacescale = user_attrib string_of_number "surfaceScale"

  let a_diffuseconstant = user_attrib string_of_number "diffuseConstant"

  let a_scale = user_attrib string_of_number "scale"

  let a_xchannelselector x =
    let f = function
      | `R -> "r"
      | `G -> "g"
      | `B -> "b"
      | `A -> "a" in
    user_attrib f "xChannelSelector" x

  let a_ychannelselector x =
    let f = function
      | `R -> "r"
      | `G -> "g"
      | `B -> "b"
      | `A -> "a" in
    user_attrib f "yChannelSelector" x

  let a_stddeviation =
    user_attrib string_of_number_optional_number "stdDeviation"

  let a_operatormorphology x =
    let f = function
      | `Erode -> "erode"
      | `Dilate -> "dilate" in
    user_attrib f "operatorMorphology" x

  let a_radius = user_attrib string_of_number_optional_number "radius"

  let a_basefrenquency =
    user_attrib string_of_number_optional_number "baseFrequency"

  let a_numoctaves = user_attrib string_of_int "numOctaves"

  let a_seed = user_attrib string_of_number "seed"

  let a_stitchtiles x =
    let f = function
      | `Stitch -> "stitch"
      | `NoStitch -> "noStitch" in
    user_attrib f "stitchTiles" x

  let a_stitchtype x =
    let f = function
      | `FractalNoise -> "fractalNoise"
      | `Turbulence -> "turbulence" in
    user_attrib f "typeStitch" x

  let a_xlinkshow x =
    let f = function
      | `New -> "new"
      | `Replace -> "replace" in
    user_attrib f "xlink:show" x

  let a_xlinkactuate x =
    let f = function
      | `OnRequest -> "onRequest" in
    user_attrib f "xlink:actuate" x

  let a_target = string_attrib "xlink:target"

  let a_viewtarget = string_attrib "viewTarget"

  let a_attributename = string_attrib "attributeName"

  let a_attributetype x =
    let f = function
      | `CSS -> "CSS"
      | `XML -> "XML"
      | `Auto -> "auto" in
    user_attrib f "attributeType" x

  let a_begin = string_attrib "begin"

  let a_dur = string_attrib "dur"

  let a_min = string_attrib "min"

  let a_max = string_attrib "max"

  let a_restart x =
    let f = function
      | `Always -> "always"
      | `WhenNotActive -> "whenNotActive"
      | `Never -> "never" in
    user_attrib f "restart" x

  let a_repeatcount = string_attrib "repeatCount"

  let a_repeatdur = string_attrib "repeatDur"

  let a_fill = user_attrib string_of_paint "fill"

  let a_fill_animation x =
    let f = function
      | `Freeze -> "freeze"
      | `Remove -> "remove" in
    user_attrib f "fill" x

  let a_calcmode x =
    let f = function
      | `Discrete -> "discrete"
      | `Linear -> "linear"
      | `Paced -> "paced"
      | `Spline -> "spline" in
    user_attrib f "calcMode" x

  let a_values_anim = Xml.comma_sep_attrib "values"

  let a_keytimes = Xml.comma_sep_attrib "keyTimes"

  let a_keysplines = Xml.comma_sep_attrib "keySplines"

  let a_from = string_attrib "from"

  let a_to = string_attrib "to"

  let a_by = string_attrib "by"

  let a_additive x =
    let f = function
      | `Replace -> "replace"
      | `Sum -> "sum" in
    user_attrib f "additive" x

  let a_accumulate x =
    let f = function
      | `None -> "none"
      | `Sum -> "sum" in
    user_attrib f "accumulate" x

  let a_keypoints = user_attrib string_of_numbers_semicolon "keyPoints"

  let a_path = string_attrib "path"

  let a_typeanimatecolor x =
    let f = function
      | `Translate -> "translate"
      | `Scale -> "scale"
      | `Rotate -> "rotate"
      | `SkewX -> "skewX"
      | `SkewY -> "skewY" in
    user_attrib f "type" x

  let a_horiz_origin_x = user_attrib string_of_number "horiz-origin-x"

  let a_horiz_origin_y = user_attrib string_of_number "horiz-origin-y"

  let a_horiz_adv_x = user_attrib string_of_number "horiz-adv-x"

  let a_vert_origin_x = user_attrib string_of_number "vert-origin-x"

  let a_vert_origin_y = user_attrib string_of_number "vert-origin-y"

  let a_vert_adv_y = user_attrib string_of_number "vert-adv-y"

  let a_unicode = string_attrib "unicode"

  let a_glyphname = string_attrib "glyphname"

  let a_orientation x =
    let f = function
      | `H -> "h"
      | `V -> "v" in
    user_attrib f "orientation" x

  let a_arabicform x =
    let f = function
      | `Initial -> "initial"
      | `Medial -> "medial"
      | `Terminal -> "terminal"
      | `Isolated -> "isolated" in
    user_attrib f "arabic-form" x

  let a_lang = string_attrib "lang"

  let a_u1 = string_attrib "u1"

  let a_u2 = string_attrib "u2"

  let a_g1 = string_attrib "g1"

  let a_g2 = string_attrib "g2"

  let a_k = string_attrib "k"

  let a_fontfamily = string_attrib "font-family"

  let a_fontstyle = string_attrib "font-style"

  let a_fontvariant = string_attrib "font-variant"

  let a_fontweight = string_attrib "font-weight"

  let a_fontstretch = string_attrib "font-stretch"

  let a_fontsize = string_attrib "font-size"

  let a_unicoderange = string_attrib "unicode-range"

  let a_unitsperem = string_attrib "units-per-em"

  let a_stemv = user_attrib string_of_number "stemv"

  let a_stemh = user_attrib string_of_number "stemh"

  let a_slope = user_attrib string_of_number "slope"

  let a_capheight = user_attrib string_of_number "cap-height"

  let a_xheight = user_attrib string_of_number "x-height"

  let a_accentheight = user_attrib string_of_number "accent-height"

  let a_ascent = user_attrib string_of_number "ascent"

  let a_widths = string_attrib "widths"

  let a_bbox = string_attrib "bbox"

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

  let a_string = string_attrib "string"

  let a_name = string_attrib "name"

  let a_onabort = Xml.event_handler_attrib "onabort"

  let a_onactivate = Xml.event_handler_attrib "onactivate"

  let a_onbegin = Xml.event_handler_attrib "onbegin"

  let a_onclick = Xml.event_handler_attrib "onclick"

  let a_onend = Xml.event_handler_attrib "onend"

  let a_onerror = Xml.event_handler_attrib "onerror"

  let a_onfocusin = Xml.event_handler_attrib "onfocusin"

  let a_onfocusout = Xml.event_handler_attrib "onfocusout"

  let a_onload = Xml.event_handler_attrib "onload"

  let a_onmousedown = Xml.event_handler_attrib "onmousdown"

  let a_onmouseup = Xml.event_handler_attrib "onmouseup"

  let a_onmouseover = Xml.event_handler_attrib "onmouseover"

  let a_onmouseout = Xml.event_handler_attrib "onmouseout"

  let a_onmousemove = Xml.event_handler_attrib "onmousemove"

  let a_onrepeat = Xml.event_handler_attrib "onrepeat"

  let a_onresize = Xml.event_handler_attrib "onresize"

  let a_onscroll = Xml.event_handler_attrib "onscroll"

  let a_onunload = Xml.event_handler_attrib "onunload"

  let a_onzoom = Xml.event_handler_attrib "onzoom"

  let a_stopcolor = user_attrib string_of_color "stop-color"

  let a_stopopacity = user_attrib string_of_number "stop-opacity"

  let a_stroke = user_attrib string_of_paint "stroke"

  let a_strokewidth = user_attrib string_of_length "stroke-width"

  let a_strokelinecap x =
    let f = function
      | `Butt -> "butt"
      | `Round -> "round"
      | `Square -> "square" in
    user_attrib f "stroke-linecap" x

  let a_strokelinejoin x =
    let f = function
      | `Miter -> "miter"
      | `Round -> "round"
      | `Bever -> "bevel" in
    user_attrib f "stroke-linejoin" x

  let a_strokemiterlimit =
    user_attrib string_of_number "stroke-miterlimit"

  let a_strokedasharray x =
    let f = function
      | [] -> "none"
      | l -> list string_of_length l in
    user_attrib f "stroke-dasharray" x

  let a_strokedashoffset =
    user_attrib string_of_length "stroke-dashoffset"

  let a_strokeopacity =
    user_attrib string_of_number "stroke-opacity"

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

  let glyphref = nullary "glyphRef"

  let marker = star "marker"

  let colorprofile = star "colorProfile"

  let lineargradient = star "linearGradient"

  let radialgradient = star "radialGradient"

  let stop = star "stop"

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

  let fefuncg = star "feFuncG"

  let fefuncb = star "feFuncB"

  let fefuncr = star "feFuncR"

  let fecomposite = star "feComposite"

  let feconvolvematrix = star "feConvolveMatrix"

  let fediffuselighting = star "feDiffuseLighting"

  let fedisplacementmap = star "feDisplacementMap"

  let feflood = star "feFlood"

  let fegaussianblur = star "feGaussianBlur"

  let feimage = star "feImage"

  let femerge = star "feMerge"

  let femorphology = star "feMorphology"

  let feoffset = star "feOffset"

  let fespecularlighting = star "feSpecularLighting"

  let fetile = star "feTile"

  let feturbulence = star "feTurbulence"

  let cursor = star "cursor"

  let a = star "a"

  let view = star "view"

  let script = unary "script"

  let animation = star "animate"

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

  module Unsafe = struct

    let data s = Xml.encodedpcdata s

    let leaf tag ?a () = Xml.leaf ?a tag

    let node tag ?a elts = Xml.node ?a tag elts

    let coerce_elt x = x

    let float_attrib = Xml.float_attrib

    let int_attrib = Xml.int_attrib

    let string_attrib = Xml.string_attrib

    let uri_attrib a s = Xml.uri_attrib a s

    let space_sep_attrib = Xml.space_sep_attrib

    let comma_sep_attrib = Xml.comma_sep_attrib

    let event_handler_attrib = Xml.event_handler_attrib

  end

end

module Make(Xml : Xml_sigs.T) =
  MakeWrapped
    (Xml_wrap.NoWrap)
    (Xml)
