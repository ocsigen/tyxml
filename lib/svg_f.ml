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
 * Foundation, Inc., 51 Franklin Street, Suite 500, Boston, MA 02110-1301, USA.
*)

(** Type instantiations for SVG *)

(** This module defines basic data types for data, attributes
    and element occurring in SVG documents.
    It is based on the specification available at http://www.w3.org/TR/SVG/.

    This module is experimental, it may lack of some attributes,
    and the interface is very low level and do not take deeply into account
    the needs of SVG elements. *)

open Svg_types

open Unit

let string_of_iri x = Printf.sprintf "url(%s)" x

module Unit = struct

  (* let rel x     = (x, None) *)
  (* let deg x     = (x, Some `Deg) *)
  (* let grad x    = (x, Some `Grad) *)
  (* let rad x     = (x, Some `Rad) *)
  (* let ms x      = (x, Some `Ms) *)
  (* let s x       = (x, Some `S) *)
  (* let em x      = (x, Some `Em) *)
  (* let ex x      = (x, Some `Ex) *)
  (* let px x      = (x, Some `Px) *)
  (* let in_ x     = (x, Some `In) *)
  (* let cm x      = (x, Some `Cm) *)
  (* let mm x      = (x, Some `Mm) *)
  (* let pt x      = (x, Some `Pt) *)
  (* let pc x      = (x, Some `Pc) *)
  (* let percent x = (x, Some `Percent) *)
  (* let hz x      = (x, Some `Hz) *)
  (* let khz x     = (x, Some `KHz) *)

  let to_string f (n, unit) = Printf.sprintf "%g%s" n begin
    match unit with
    | Some unit -> f unit
    | None -> ""
  end

  let angle_names = function `Deg -> "deg" | `Grad -> "grad" | `Rad -> "rad"
  let string_of_angle a = to_string angle_names a

  (* let time_names = function `Ms -> "ms" | `S -> "s" *)
  (* let string_of_time a = to_string time_names a *)

  let length_names = function
    | `Em -> "em" | `Ex -> "ex" | `Px -> "px" | `In -> "in" | `Cm -> "cm"
    | `Mm -> "mm" | `Pt -> "pt" | `Pc -> "pc" | `Percent -> "%"
  let string_of_length (a: length) = to_string length_names a

  (* let freq_names = function `Hz -> "Hz" | `KHz -> "kHz" *)
  (* let string_of_freq a = to_string freq_names a *)

end

open Unit

let opt_concat ?(sep=" ") s f = function
  | Some x -> s ^ sep ^ (f x)
  | None -> s

let list ?(sep=" ") f l = String.concat sep (List.map f l)

let string_of_color s = s
(* For now just string, we may want something better in the future. *)

let string_of_icccolor s = s

let string_of_paint_without_icc = function
  | `None -> "none"
  | `CurrentColor -> "currentColor"
  | `Color (c, icc) -> opt_concat (string_of_color c) string_of_icccolor icc

let string_of_paint = function
  | `Icc (iri, None) -> string_of_iri iri
  | `Icc (iri, Some b) ->
    (string_of_iri iri) ^" "^ (string_of_paint_without_icc b)
  | #paint_without_icc as c -> string_of_paint_without_icc c

let string_of_fill_rule = function
  | `Nonzero -> "nonzero"
  | `Evenodd -> "evenodd"

module Make_with_wrapped_functions

    (Xml : Xml_sigs.T)
    (C : Svg_sigs.Wrapped_functions with module Xml = Xml) =

struct

  module Xml = Xml
  module W = Xml.W

  module Info = struct
    let content_type = "image/svg+xml"
    let alternative_content_types = []
    let emptytags = []
    let version = "SVG 1.1"
    let standard = "http://www.w3.org/TR/svg11/"
    let namespace = "http://www.w3.org/2000/svg"
    let doctype =
      Xml_print.compose_doctype"svg"
        ["-//W3C//DTD SVG 1.1//EN";
         "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"]
  end

  type uri = Xml.uri
  let string_of_uri = Xml.string_of_uri
  let uri_of_string = Xml.uri_of_string


  (* Mandatory XML stuff. *)

  type 'a attrib = Xml.attrib

  type +'a elt = Xml.elt

  type 'a wrap = 'a W.t
  type 'a list_wrap = 'a W.tlist

  type ('a, 'b) nullary = ?a: (('a attrib) list) -> unit -> 'b elt

  type ('a, 'b, 'c) unary = ?a: (('a attrib) list) -> 'b elt wrap -> 'c elt

  type ('a, 'b, 'c) star =
    ?a: (('a attrib) list) -> ('b elt) list_wrap -> 'c elt

  let tot x = x

  let totl x = x

  let toelt x = x

  let toeltl x = x

  let to_attrib x = x

  let nullary tag ?a () =
    Xml.node ?a tag (W.nil ())

  let unary tag ?a elt =
    Xml.node ?a tag (W.singleton elt)

  let star tag ?a elts = Xml.node ?a tag elts

  type altglyphdef_content =
    [ `Ref of (glyphref elt) list
    | `Item of (altglyphitem elt) list
    ]

  let to_xmlattribs x = x

  let float_attrib = Xml.float_attrib

  let string_attrib = Xml.string_attrib

  (* wrap C module functions *)

  let string_of_coord = C.string_of_length

  let string_of_length = C.string_of_length

  let string_of_lengths = C.string_of_lengths

  (* Custom XML attributes *)

  let user_attrib f name v =
    Xml.string_attrib name (W.fmap f v)

  let number_attrib = float_attrib

  (* for now string_attrib, we may want something better in the
     future. *)
  let color_attrib = Xml.string_attrib

  (* SVG attributes *)

  let metadata ?a children = Xml.node ?a "metadata" children

  let foreignObject ?a children = Xml.node ?a "foreignObject" children

  let txt s = Xml.pcdata s
  let pcdata = txt

  (* generated *)
  let a_version = string_attrib "version"

  let a_baseProfile = string_attrib "baseProfile"

  let a_x = user_attrib string_of_coord "x"

  let a_y = user_attrib string_of_coord "y"

  let a_width = user_attrib string_of_length "width"

  let a_height = user_attrib string_of_length "height"

  let a_preserveAspectRatio =
    string_attrib "preserveAspectRatio"

  let a_crossorigin x = user_attrib C.string_of_big_variant "crossorigin" x

  let a_decoding x = user_attrib C.string_of_big_variant "decoding" x

  let a_fetchpriority x = user_attrib C.string_of_big_variant "fetchpriority" x

  let a_contentScriptType =
    string_attrib "contentScriptType"

  let a_contentStyleType = string_attrib "contentStyleType"

  let a_zoomAndPan x =
    user_attrib C.string_of_big_variant "zoomAndPan" x

  let a_href = string_attrib "href"

  let a_xlink_href = string_attrib "xlink:href"

  let a_requiredFeatures =
    Xml.space_sep_attrib "requiredFeatures"

  let a_requiredExtensions =
    Xml.space_sep_attrib "requiredExtensions"

  let a_systemLanguage =
    Xml.comma_sep_attrib "systemLanguage"

  let a_externalResourcesRequired =
    user_attrib C.string_of_bool "externalResourcesRequired"

  let a_id = string_attrib "id"

  let a_user_data name = string_attrib ("data-" ^ name)

  let a_autofocus () = string_attrib "autofocus" (W.return "autofocus")

  let a_tabindex = user_attrib C.string_of_int "tabindex"

  let a_lang = string_attrib "lang"

  let a_xml_base = string_attrib "xml:base"

  let a_xml_lang = string_attrib "xml:lang"

  let a_xml_space x =
    user_attrib C.string_of_big_variant "xml:space" x

  let a_type = string_attrib "type"

  let a_media = Xml.comma_sep_attrib "media"

  let a_title = string_attrib "title"

  let a_xlink_title = string_attrib "xlink:title"

  let a_xlink_type = string_attrib "xlink:type"

  let a_xlink_role = string_attrib "xlink:role"

  let a_xlink_arcrole = string_attrib "xlink:arcrole"

  let a_class = Xml.space_sep_attrib "class"

  let a_style = string_attrib "style"

  let a_role = Xml.space_sep_attrib "role"

  let a_aria name = Xml.space_sep_attrib ("aria-" ^ name)

  let a_transform = user_attrib C.string_of_transforms "transform"

  let a_viewBox = user_attrib C.string_of_fourfloats "viewBox"

  let a_d = string_attrib "d"

  let a_pathLength = number_attrib "pathLength"

  let a_rx = user_attrib string_of_length "rx"

  let a_ry = user_attrib string_of_length "ry"

  let a_cx = user_attrib string_of_length "cx"

  let a_cy = user_attrib string_of_length "cy"

  let a_r = user_attrib string_of_length "r"

  let a_x1 = user_attrib string_of_coord "x1"

  let a_y1 = user_attrib string_of_coord "y1"

  let a_x2 = user_attrib string_of_coord "x2"

  let a_y2 = user_attrib string_of_coord "y2"

  let a_points = user_attrib C.string_of_coords "points"

  let a_x_list = user_attrib string_of_lengths "x"

  let a_y_list = user_attrib string_of_lengths "y"

  let a_dx = user_attrib C.string_of_number "dx"

  let a_dy = user_attrib C.string_of_number "dy"

  let a_dx_list = user_attrib string_of_lengths "dx"

  let a_dy_list = user_attrib string_of_lengths "dy"

  let a_lengthAdjust x =
    user_attrib C.string_of_big_variant "lengthAdjust" x

  let a_textLength = user_attrib string_of_length "textLength"

  let a_text_anchor x =
    user_attrib C.string_of_big_variant "text-anchor" x

  let a_text_decoration x =
    user_attrib C.string_of_big_variant "text-decoration" x

  let a_text_rendering x =
    user_attrib C.string_of_big_variant "text-rendering" x

  let a_rotate = user_attrib C.string_of_numbers "rotate"

  let a_startOffset = user_attrib string_of_length "startOffset"

  let a_method x =
    user_attrib C.string_of_big_variant "method" x

  let a_spacing x =
    user_attrib C.string_of_big_variant "spacing" x

  let a_side x = user_attrib C.string_of_big_variant "side" x

  let a_glyphRef = string_attrib "glyphRef"

  let a_format = string_attrib "format"

  let a_markerUnits x =
    user_attrib C.string_of_big_variant "markerUnits" x

  let a_refX = user_attrib string_of_coord "refX"

  let a_refY = user_attrib string_of_coord "refY"

  let a_markerWidth = user_attrib string_of_length "markerWidth"

  let a_markerHeight = user_attrib string_of_length "markerHeight"

  let a_orient x =
    user_attrib C.string_of_orient "orient" x

  let a_local = string_attrib "local"

  let a_rendering_intent x =
    user_attrib C.string_of_big_variant "rendering-intent" x

  let a_gradientUnits x =
    user_attrib C.string_of_big_variant "gradientUnits" x

  let a_gradientTransform =
    user_attrib C.string_of_transforms "gradientTransform"

  let a_spreadMethod x =
    user_attrib C.string_of_big_variant "spreadMethod" x

  let a_fx = user_attrib string_of_coord "fx"

  let a_fy = user_attrib string_of_coord "fy"

  let a_fr = user_attrib string_of_length "fr"

  let a_offset x =
    user_attrib C.string_of_offset "offset" x

  let a_patternUnits x =
    user_attrib C.string_of_big_variant "patternUnits" x

  let a_patternContentUnits x =
    user_attrib C.string_of_big_variant "patternContentUnits" x

  let a_patternTransform x =
    user_attrib C.string_of_transforms "patternTransform" x

  let a_clip_path =
    string_attrib "clip-path"

  let a_clipPathUnits x =
    user_attrib C.string_of_big_variant "clipPathUnits" x

  let a_maskUnits x =
    user_attrib C.string_of_big_variant "maskUnits" x

  let a_maskContentUnits x =
    user_attrib C.string_of_big_variant "maskContentUnits" x

  let a_filterUnits x =
    user_attrib C.string_of_big_variant "filterUnits" x

  let a_primitiveUnits x =
    user_attrib C.string_of_big_variant "primitiveUnits" x

  let a_filterRes =
    user_attrib C.string_of_number_optional_number "filterRes"

  let a_result = string_attrib "result"

  let a_in x =
    user_attrib C.string_of_in_value "in" x

  let a_in2 x =
    user_attrib C.string_of_in_value "in2" x

  let a_azimuth = number_attrib "azimuth"

  let a_elevation = number_attrib "elevation"

  let a_z = user_attrib C.string_of_number "z"

  let a_pointsAtX = number_attrib "pointsAtX"

  let a_pointsAtY = number_attrib "pointsAtY"

  let a_pointsAtZ = number_attrib "pointsAtZ"

  let a_specularExponent = number_attrib "specularExponent"

  let a_specularConstant = number_attrib "specularConstant"

  let a_limitingConeAngle = number_attrib "limitingConeAngle"

  let a_mode x =
    user_attrib C.string_of_big_variant "mode" x

  let a_feColorMatrix_type x =
    user_attrib C.string_of_big_variant "type" x

  let a_values = user_attrib C.string_of_numbers "values"

  let a_transfer_type x =
    user_attrib C.string_of_big_variant "type" x

  let a_tableValues = user_attrib C.string_of_numbers "tableValues"

  let a_intercept = user_attrib C.string_of_number "intercept"

  let a_amplitude = user_attrib C.string_of_number "amplitude"

  let a_exponent = user_attrib C.string_of_number "exponent"

  let a_transfer_offset = user_attrib C.string_of_number "offset"

  let a_feComposite_operator x =
    user_attrib C.string_of_big_variant "operator" x

  let a_k1 = user_attrib C.string_of_number "k1"

  let a_k2 = user_attrib C.string_of_number "k2"

  let a_k3 = user_attrib C.string_of_number "k3"

  let a_k4 = user_attrib C.string_of_number "k4"

  let a_order = user_attrib C.string_of_number_optional_number "order"

  let a_kernelMatrix = user_attrib C.string_of_numbers "kernelMatrix"

  let a_divisor = user_attrib C.string_of_number "divisor"

  let a_bias = user_attrib C.string_of_number "bias"

  let a_kernelUnitLength =
    user_attrib C.string_of_number_optional_number "kernelUnitLength"

  let a_targetX = user_attrib C.string_of_int "targetX"

  let a_targetY = user_attrib C.string_of_int "targetY"

  let a_edgeMode x =
    user_attrib C.string_of_big_variant "edgeMode" x

  let a_preserveAlpha = user_attrib C.string_of_bool "preserveAlpha"

  let a_surfaceScale = user_attrib C.string_of_number "surfaceScale"

  let a_diffuseConstant =
    user_attrib C.string_of_number "diffuseConstant"

  let a_scale = user_attrib C.string_of_number "scale"

  let a_xChannelSelector x =
    user_attrib C.string_of_big_variant "xChannelSelector" x

  let a_yChannelSelector x =
    user_attrib C.string_of_big_variant "yChannelSelector" x

  let a_stdDeviation =
    user_attrib C.string_of_number_optional_number "stdDeviation"

  let a_feMorphology_operator x =
    user_attrib C.string_of_big_variant "operator" x

  let a_radius = user_attrib C.string_of_number_optional_number "radius"

  let a_baseFrequency =
    user_attrib C.string_of_number_optional_number "baseFrequency"

  let a_numOctaves = user_attrib C.string_of_int "numOctaves"

  let a_seed = user_attrib C.string_of_number "seed"

  let a_stitchTiles x =
    user_attrib C.string_of_big_variant "stitchTiles" x

  let a_feTurbulence_type x =
    user_attrib C.string_of_big_variant "type" x

  let a_xlink_show x =
    user_attrib C.string_of_big_variant "xlink:show" x

  let a_xlink_actuate x =
    user_attrib C.string_of_big_variant "xlink:actuate" x

  let a_target = string_attrib "target"

  let a_download file = user_attrib C.unoption_string "download" file

  let a_hreflang = string_attrib "hreflang"

  let a_ping = Xml.space_sep_attrib "ping"

  let a_referrerpolicy x =
    user_attrib C.string_of_big_variant "referrerpolicy" x

  let a_rel = Xml.space_sep_attrib "rel"

  let a_viewTarget = string_attrib "viewTarget"

  let a_attributeName = string_attrib "attributeName"

  let a_attributeType x =
    user_attrib C.string_of_big_variant "attributeType" x

  let a_begin = string_attrib "begin"

  let a_dur = string_attrib "dur"

  let a_end = string_attrib "end"

  let a_min = string_attrib "min"

  let a_max = string_attrib "max"

  let a_restart x =
    user_attrib C.string_of_big_variant "restart" x

  let a_repeatCount = string_attrib "repeatCount"

  let a_repeatDur = string_attrib "repeatDur"

  let a_fill = user_attrib C.string_of_paint "fill"

  let a_animation_fill x =
    user_attrib C.string_of_big_variant "fill" x

  let a_fill_rule = user_attrib C.string_of_fill_rule "fill-rule"

  let a_calcMode x =
    user_attrib C.string_of_big_variant "calcMode" x

  let a_animation_values = Xml.comma_sep_attrib "values"

  let a_keyTimes = Xml.comma_sep_attrib "keyTimes"

  let a_keySplines = Xml.comma_sep_attrib "keySplines"

  let a_from = string_attrib "from"

  let a_to = string_attrib "to"

  let a_by = string_attrib "by"

  let a_additive x =
    user_attrib C.string_of_big_variant "additive" x

  let a_accumulate x =
    user_attrib C.string_of_big_variant "accumulate" x

  let a_keyPoints = user_attrib C.string_of_numbers_semicolon "keyPoints"

  let a_origin = string_attrib "origin"

  let a_path = string_attrib "path"

  let a_animateTransform_type =
    user_attrib C.string_of_big_variant "type"

  let a_horiz_origin_x = user_attrib C.string_of_number "horiz-origin-x"

  let a_horiz_origin_y = user_attrib C.string_of_number "horiz-origin-y"

  let a_horiz_adv_x = user_attrib C.string_of_number "horiz-adv-x"

  let a_vert_origin_x = user_attrib C.string_of_number "vert-origin-x"

  let a_vert_origin_y = user_attrib C.string_of_number "vert-origin-y"

  let a_vert_adv_y = user_attrib C.string_of_number "vert-adv-y"

  let a_unicode = string_attrib "unicode"

  let a_glyph_name = string_attrib "glyph-name"

  let a_orientation x =
    user_attrib C.string_of_big_variant "orientation" x

  let a_arabic_form x =
    user_attrib C.string_of_big_variant "arabic-form" x

  let a_u1 = string_attrib "u1"

  let a_u2 = string_attrib "u2"

  let a_g1 = string_attrib "g1"

  let a_g2 = string_attrib "g2"

  let a_k = string_attrib "k"

  let a_font_family = string_attrib "font-family"

  let a_font_style = string_attrib "font-style"

  let a_font_variant = string_attrib "font-variant"

  let a_font_weight = string_attrib "font-weight"

  let a_font_stretch = string_attrib "font-stretch"

  let a_font_size = string_attrib "font-size"

  let a_unicode_range = string_attrib "unicode-range"

  let a_units_per_em = string_attrib "units-per-em"

  let a_panose_1 = string_attrib "panose-1"

  let a_stemv = user_attrib C.string_of_number "stemv"

  let a_stemh = user_attrib C.string_of_number "stemh"

  let a_slope = user_attrib C.string_of_number "slope"

  let a_cap_height = user_attrib C.string_of_number "cap-height"

  let a_x_height = user_attrib C.string_of_number "x-height"

  let a_accent_height = user_attrib C.string_of_number "accent-height"

  let a_ascent = user_attrib C.string_of_number "ascent"

  let a_descent = user_attrib C.string_of_number "descent"

  let a_widths = string_attrib "widths"

  let a_bbox = string_attrib "bbox"

  let a_ideographic = user_attrib C.string_of_number "ideographic"

  let a_alphabetic = user_attrib C.string_of_number "alphabetic"

  let a_mathematical = user_attrib C.string_of_number "mathematical"

  let a_hanging = user_attrib C.string_of_number "hanging"

  let a_videographic = user_attrib C.string_of_number "v-ideographic"

  let a_v_alphabetic = user_attrib C.string_of_number "v-alphabetic"

  let a_v_mathematical = user_attrib C.string_of_number "v-mathematical"

  let a_v_hanging = user_attrib C.string_of_number "v-hanging"

  let a_underline_position =
    user_attrib C.string_of_number "underline-position"

  let a_underline_thickness =
    user_attrib C.string_of_number "underline-thickness"

  let a_strikethrough_position =
    user_attrib C.string_of_number "strikethrough-position"

  let a_strikethrough_thickness =
    user_attrib C.string_of_number "strikethrough-thickness"

  let a_overline_position = user_attrib C.string_of_number "overline-position"

  let a_overline_thickness =
    user_attrib C.string_of_number "overline-thickness"

  let a_string = string_attrib "string"

  let a_name = string_attrib "name"

  let a_alignment_baseline x =
    user_attrib C.string_of_alignment_baseline "alignment-baseline" x

  let a_dominant_baseline x =
    user_attrib C.string_of_dominant_baseline "dominant-baseline" x

  (** Javascript events *)

  let a_onabort = Xml.event_handler_attrib "onabort"
  let a_onactivate = Xml.event_handler_attrib "onactivate"
  let a_onbegin = Xml.event_handler_attrib "onbegin"
  let a_onend = Xml.event_handler_attrib "onend"
  let a_onerror = Xml.event_handler_attrib "onerror"
  let a_onfocusin = Xml.event_handler_attrib "onfocusin"
  let a_onfocusout = Xml.event_handler_attrib "onfocusout"
  let a_onload = Xml.event_handler_attrib "onload"
  let a_onrepeat = Xml.event_handler_attrib "onrepeat"
  let a_onresize = Xml.event_handler_attrib "onresize"
  let a_onscroll = Xml.event_handler_attrib "onscroll"
  let a_onunload = Xml.event_handler_attrib "onunload"
  let a_onzoom = Xml.event_handler_attrib "onzoom"

  let a_onafterprint = Xml.event_handler_attrib "onafterprint"
  let a_onbeforeprint = Xml.event_handler_attrib "onbeforeprint"
  let a_onbeforeunload = Xml.event_handler_attrib "onbeforeunload"
  let a_onhashchange = Xml.event_handler_attrib "onhashchange"
  let a_onlanguagechange = Xml.event_handler_attrib "onlanguagechange"
  let a_onmessage = Xml.event_handler_attrib "onmessage"
  let a_onoffline = Xml.event_handler_attrib "onoffline"
  let a_ononline = Xml.event_handler_attrib "ononline"
  let a_onpagehide = Xml.event_handler_attrib "onpagehide"
  let a_onpageshow = Xml.event_handler_attrib "onpageshow"
  let a_onpopstate = Xml.event_handler_attrib "onpopstate"
  let a_onreadystatechange = Xml.event_handler_attrib "onreadystatechange"
  let a_onredo = Xml.event_handler_attrib "onredo"
  let a_onrejectionhandled = Xml.event_handler_attrib "onrejectionhandled"
  let a_onstorage = Xml.event_handler_attrib "onstorage"
  let a_onundo = Xml.event_handler_attrib "onundo"
  let a_onunhandledrejection = Xml.event_handler_attrib "onunhandledrejection"

  (** Javascript mouse events *)

  let a_onauxclick = Xml.mouse_event_handler_attrib "onauxclick"
  let a_onbeforeinput = Xml.event_handler_attrib "onbeforeinput"
  let a_onbeforematch = Xml.event_handler_attrib "onbeforematch"
  let a_onbeforetoggle = Xml.event_handler_attrib "onbeforetoggle"
  let a_onblur = Xml.event_handler_attrib "onblur"
  let a_oncancel = Xml.event_handler_attrib "oncancel"
  let a_oncanplay = Xml.event_handler_attrib "oncanplay"
  let a_oncanplaythrough = Xml.event_handler_attrib "oncanplaythrough"
  let a_onchange = Xml.event_handler_attrib "onchange"
  let a_onclose = Xml.event_handler_attrib "onclose"
  let a_oncontextlost = Xml.event_handler_attrib "oncontextlost"
  let a_oncontextmenu = Xml.mouse_event_handler_attrib "oncontextmenu"
  let a_oncontextrestored = Xml.event_handler_attrib "oncontextrestored"
  let a_oncopy = Xml.event_handler_attrib "oncopy"
  let a_oncuechange = Xml.event_handler_attrib "oncuechange"
  let a_oncut = Xml.event_handler_attrib "oncut"
  let a_ondblclick = Xml.mouse_event_handler_attrib "ondblclick"
  let a_ondrag = Xml.mouse_event_handler_attrib "ondrag"
  let a_ondragend = Xml.mouse_event_handler_attrib "ondragend"
  let a_ondragenter = Xml.mouse_event_handler_attrib "ondragenter"
  let a_ondragleave = Xml.mouse_event_handler_attrib "ondragleave"
  let a_ondragover = Xml.mouse_event_handler_attrib "ondragover"
  let a_ondragstart = Xml.mouse_event_handler_attrib "ondragstart"
  let a_ondrop = Xml.mouse_event_handler_attrib "ondrop"
  let a_ondurationchange = Xml.event_handler_attrib "ondurationchange"
  let a_onemptied = Xml.event_handler_attrib "onemptied"
  let a_onended = Xml.event_handler_attrib "onended"
  let a_onfocus = Xml.event_handler_attrib "onfocus"
  let a_ongotpointercapture =
    Xml.mouse_event_handler_attrib "ongotpointercapture"
  let a_oninput = Xml.event_handler_attrib "oninput"
  let a_oninvalid = Xml.event_handler_attrib "oninvalid"
  let a_onkeydown = Xml.keyboard_event_handler_attrib "onkeydown"
  let a_onkeypress = Xml.keyboard_event_handler_attrib "onkeypress"
  let a_onkeyup = Xml.keyboard_event_handler_attrib "onkeyup"
  let a_onloadeddata = Xml.event_handler_attrib "onloadeddata"
  let a_onloadedmetadata = Xml.event_handler_attrib "onloadedmetadata"
  let a_onloadstart = Xml.event_handler_attrib "onloadstart"
  let a_onlostpointercapture =
    Xml.mouse_event_handler_attrib "onlostpointercapture"
  let a_onmousewheel = Xml.event_handler_attrib "onmousewheel"
  let a_onpaste = Xml.event_handler_attrib "onpaste"
  let a_onpause = Xml.event_handler_attrib "onpause"
  let a_onplay = Xml.event_handler_attrib "onplay"
  let a_onplaying = Xml.event_handler_attrib "onplaying"
  let a_onpointercancel = Xml.mouse_event_handler_attrib "onpointercancel"
  let a_onpointerdown = Xml.mouse_event_handler_attrib "onpointerdown"
  let a_onpointerenter = Xml.mouse_event_handler_attrib "onpointerenter"
  let a_onpointerleave = Xml.mouse_event_handler_attrib "onpointerleave"
  let a_onpointermove = Xml.mouse_event_handler_attrib "onpointermove"
  let a_onpointerout = Xml.mouse_event_handler_attrib "onpointerout"
  let a_onpointerover = Xml.mouse_event_handler_attrib "onpointerover"
  let a_onpointerup = Xml.mouse_event_handler_attrib "onpointerup"
  let a_onprogress = Xml.event_handler_attrib "onprogress"
  let a_onratechange = Xml.event_handler_attrib "onratechange"
  let a_onscrollend = Xml.event_handler_attrib "onscrollend"
  let a_onsecuritypolicyviolation =
    Xml.event_handler_attrib "onsecuritypolicyviolation"
  let a_onseeked = Xml.event_handler_attrib "onseeked"
  let a_onseeking = Xml.event_handler_attrib "onseeking"
  let a_onselect = Xml.event_handler_attrib "onselect"
  let a_onshow = Xml.event_handler_attrib "onshow"
  let a_onstalled = Xml.event_handler_attrib "onstalled"
  let a_onsubmit = Xml.event_handler_attrib "onsubmit"
  let a_onsuspend = Xml.event_handler_attrib "onsuspend"
  let a_ontimeupdate = Xml.event_handler_attrib "ontimeupdate"
  let a_ontoggle = Xml.event_handler_attrib "ontoggle"
  let a_onvolumechange = Xml.event_handler_attrib "onvolumechange"
  let a_onwaiting = Xml.event_handler_attrib "onwaiting"
  let a_onwheel = Xml.mouse_event_handler_attrib "onwheel"

  let a_onclick = Xml.mouse_event_handler_attrib "onclick"
  let a_onmousedown = Xml.mouse_event_handler_attrib "onmousedown"
  let a_onmouseup = Xml.mouse_event_handler_attrib "onmouseup"
  let a_onmouseover = Xml.mouse_event_handler_attrib "onmouseover"
  let a_onmouseout = Xml.mouse_event_handler_attrib "onmouseout"
  let a_onmousemove = Xml.mouse_event_handler_attrib "onmousemove"

  (** Javascript touch events *)
  let a_ontouchstart = Xml.touch_event_handler_attrib "ontouchstart"
  let a_ontouchend = Xml.touch_event_handler_attrib "ontouchend"
  let a_ontouchmove = Xml.touch_event_handler_attrib "ontouchmove"
  let a_ontouchcancel = Xml.touch_event_handler_attrib "ontouchcancel"

  let a_stop_color = color_attrib "stop-color"

  let a_stop_opacity = user_attrib C.string_of_number "stop-opacity"

  let a_stroke = user_attrib C.string_of_paint "stroke"

  let a_stroke_width = user_attrib C.string_of_length "stroke-width"

  let a_stroke_linecap x =
    user_attrib C.string_of_big_variant "stroke-linecap" x

  let a_stroke_linejoin x =
    user_attrib C.string_of_big_variant "stroke-linejoin" x

  let a_stroke_miterlimit =
    user_attrib C.string_of_number "stroke-miterlimit"

  let a_stroke_dasharray x =
    user_attrib C.string_of_strokedasharray "stroke-dasharray" x

  let a_stroke_dashoffset =
    user_attrib C.string_of_length "stroke-dashoffset"

  let a_stroke_opacity =
    user_attrib C.string_of_number "stroke-opacity"

  let a_baseline_shift = string_attrib "baseline-shift"

  let a_clip = string_attrib "clip"

  let a_clip_rule x = user_attrib C.string_of_big_variant "clip-rule" x

  let a_color = color_attrib "color"

  let a_color_interpolation x =
    user_attrib C.string_of_big_variant "color-interpolation" x

  let a_color_interpolation_filters x =
    user_attrib C.string_of_big_variant "color-interpolation-filters" x

  let a_color_profile = string_attrib "color-profile"

  let a_color_rendering x =
    user_attrib C.string_of_big_variant "color-rendering" x

  let a_cursor = string_attrib "cursor"

  let a_direction x = user_attrib C.string_of_big_variant "direction" x

  let a_display = string_attrib "display"

  let a_enable_background x =
    user_attrib C.string_of_big_variant "enable-background" x

  let a_fill_opacity = user_attrib C.string_of_number "fill-opacity"

  let a_filter = string_attrib "filter"

  let a_flood_color = color_attrib "flood-color"

  let a_flood_opacity = user_attrib C.string_of_number "flood-opacity"

  let a_font_size_adjust = string_attrib "font-size-adjust"

  let a_glyph_orientation_horizontal =
    string_attrib "glyph-orientation-horizontal"

  let a_glyph_orientation_vertical = string_attrib "glyph-orientation-vertical"

  let a_image_rendering x =
    user_attrib C.string_of_big_variant "image-rendering" x

  let a_kerning = string_attrib "kerning"

  let a_letter_spacing = string_attrib "letter-spacing"

  let a_lighting_color = color_attrib "lighting-color"

  let a_marker_end = string_attrib "marker-end"

  let a_marker_mid = string_attrib "marker-mid"

  let a_marker_start = string_attrib "marker-start"

  let a_mask = string_attrib "mask"

  let a_opacity = user_attrib C.string_of_number "opacity"

  let a_overflow x = user_attrib C.string_of_big_variant "overflow" x

  let a_pointer_events x =
    user_attrib C.string_of_big_variant "pointer-events" x

  let a_shape_rendering x =
    user_attrib C.string_of_big_variant "shape-rendering" x

  let a_unicode_bidi x = user_attrib C.string_of_big_variant "unicode-bidi" x

  let a_visibility x = user_attrib C.string_of_big_variant "visibility" x

  let a_word_spacing = string_attrib "word-spacing"

  let a_writing_mode x = user_attrib C.string_of_big_variant "writing-mode" x

  let a_paint_order = string_attrib "paint-order"

  let a_text_overflow x = user_attrib C.string_of_big_variant "text-overflow" x

  let a_transform_origin = string_attrib "transform-origin"

  let a_vector_effect x = user_attrib C.string_of_big_variant "vector-effect" x

  let a_white_space x = user_attrib C.string_of_big_variant "white-space" x

  (* xlink namespace given a nickname since some attributes mandated by
     the svg standard such as xlink:href live in that namespace, and we
     refer to them as "xlink:whatever" (see a_xlink_href or a_xlinkshow)
  *)
  let svg ?(a = []) children =
    let attribs =
      string_attrib "xmlns" (W.return "http://www.w3.org/2000/svg")
      :: string_attrib "xmlns:xlink" (W.return "http://www.w3.org/1999/xlink")
      :: to_xmlattribs a
    in
    star ~a:(attribs) "svg" children

  (* also generated *)
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

  let textPath = star "textPath"

  let altGlyph = unary "altGlyph"

  let altGlyphDef = unary "altGlyphDef"

  let altGlyphItem = star "altGlyphItem"

  let glyphRef = nullary "glyphRef"

  let marker = star "marker"

  let color_profile = star "color-profile"

  let linearGradient = star "linearGradient"

  let radialGradient = star "radialGradient"

  let stop = star "stop"

  let pattern = star "pattern"

  let clipPath = star "clipPath"

  let mask = star "mask"

  let filter = star "filter"

  let feDistantLight = star "feDistantLight"

  let fePointLight = star "fePointLight"

  let feSpotLight = star "feSpotLight"

  let feBlend = star "feBlend"

  let feColorMatrix = star "feColorMatrix"

  let feComponentTransfer = star "feComponentTransfer"

  let feFuncA = star "feFuncA"

  let feFuncG = star "feFuncG"

  let feFuncB = star "feFuncB"

  let feFuncR = star "feFuncR"

  let feComposite = star "feComposite"

  let feConvolveMatrix = star "feConvolveMatrix"

  let feDiffuseLighting = star "feDiffuseLighting"

  let feDisplacementMap = star "feDisplacementMap"

  let feDropShadow = star "feDropShadow"

  let feFlood = star "feFlood"

  let feGaussianBlur = star "feGaussianBlur"

  let feImage = star "feImage"

  let feMerge = star "feMerge"

  let feMergeNode = star "feMergeNode"

  let feMorphology = star "feMorphology"

  let feOffset = star "feOffset"

  let feSpecularLighting = star "feSpecularLighting"

  let feTile = star "feTile"

  let feTurbulence = star "feTurbulence"

  let cursor = star "cursor"

  let a = star "a"

  let view = star "view"

  let script = unary "script"

  let animate = star "animate"

  let animation = animate

  let set = star "set"

  let animateMotion = star "animateMotion"

  let mpath = star "mpath"

  let animateColor = star "animateColor"

  let animateTransform = star "animateTransform"

  let font = star "font"

  let glyph = star "glyph"

  let missing_glyph = star "missing-glyph"

  let hkern = nullary "hkern"

  let vkern = nullary "vkern"

  let font_face = nullary "font-face"

  let font_face_src = star "font-face-src"

  let font_face_uri = star "font-face-uri"

  let font_face_format = nullary "font-face-format"

  let font_face_name = nullary "font-face-name"

  type doc = [ `Svg ] elt
  let doc_toelt x = x

  module I = Xml_stream.Import(Xml)
  let of_seq s = totl @@ I.of_seq s

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

  end

end

module Wrapped_functions
    (Xml : Xml_sigs.T with type ('a,'b) W.ft = 'a -> 'b) =
struct

  module Xml = Xml

  let string_of_alignment_baseline = function
    | `Auto -> "auto"
    | `Baseline -> "baseline"
    | `Before_edge -> "before-edge"
    | `Text_before_edge -> "text-before-edge"
    | `Middle -> "middle"
    | `Central -> "central"
    | `After_edge -> "after-edge"
    | `Text_after_edge -> "text-after-edge"
    | `Ideographic -> "ideographic"
    | `Alphabetic -> "alphabetic"
    | `Hanging-> "hanging"
    | `Mathematical -> "mathematical"
    | `Text_bottom -> "text-bottom"
    | `Text_top -> "text-top"
    | `Inherit -> "inherit"

  let string_of_big_variant = function
    | `A -> "a"
    | `Absolute_colorimetric -> "absolute-colorimetric"
    | `Accumulate -> "accumulate"
    | `Align -> "align"
    | `All -> "all"
    | `Always -> "always"
    | `Anonymous -> "anonymous"
    | `Arcs -> "arcs"
    | `Arithmetic -> "arithmetic"
    | `Async -> "async"
    | `Atop -> "atop"
    | `Auto -> "auto"
    | `B -> "b"
    | `Bevel -> "bevel"
    | `Bidi_override -> "bidi-override"
    | `Blink -> "blink"
    | `Break_spaces -> "break-spaces"
    | `Butt -> "butt"
    | `Clip -> "clip"
    | `Collapse -> "collapse"
    | `Color -> "color"
    | `Color_burn -> "color-burn"
    | `Color_dodge -> "color-dodge"
    | `CrispEdges -> "crispEdges"
    | `CSS -> "CSS"
    | `Darken -> "darken"
    | `Default -> "default"
    | `Difference -> "difference"
    | `Dilate -> "dilate"
    | `Disable -> "disable"
    | `Discrete -> "discrete"
    | `Duplicate -> "duplicate"
    | `Ellipsis -> "ellipsis"
    | `Embed -> "embed"
    | `End -> "end"
    | `Erode -> "erode"
    | `Evenodd -> "evenodd"
    | `Exact -> "exact"
    | `Exclusion -> "exclusion"
    | `Fill -> "fill"
    | `Fixed_position -> "fixed-position"
    | `FractalNoise -> "fractalNoise"
    | `Freeze -> "freeze"
    | `G -> "g"
    | `Gamma -> "gamma"
    | `GeometricPrecision -> "geometricPrecision"
    | `H -> "h"
    | `Hard_light -> "hard-light"
    | `Hidden -> "hidden"
    | `High -> "high"
    | `Horizontal_tb -> "horizontal-tb"
    | `Hue -> "hue"
    | `HueRotate -> "hueRotate"
    | `Identity -> "identity"
    | `In -> "in"
    | `Inherit -> "inherit"
    | `Initial -> "initial"
    | `Isolated -> "isolated"
    | `Left -> "left"
    | `Lighten -> "lighten"
    | `Line_through -> "line-through"
    | `Linear -> "linear"
    | `LinearRGB -> "linearRGB"
    | `Low -> "low"
    | `Ltr -> "ltr"
    | `LuminanceToAlpha -> "luminanceToAlpha"
    | `Luminosity -> "luminosity"
    | `Magnify -> "magnify"
    | `Matrix -> "matrix"
    | `Medial -> "medial"
    | `Middle -> "middle"
    | `Miter -> "miter"
    | `Miter_clip -> "miter-clip"
    | `Multiply -> "multiply"
    | `Never -> "never"
    | `New -> "new"
    | `No_referrer -> "no-referrer"
    | `No_referrer_when_downgrade -> "no-referrer-when-downgrade"
    | `Non_rotation -> "non-rotation"
    | `Non_scaling_size -> "non-scaling-size"
    | `Non_scaling_stroke -> "non-scaling-stroke"
    | `None -> "none"
    | `Nonzero -> "nonzero"
    | `Normal -> "normal"
    | `NoStitch -> "noStitch"
    | `Nowrap -> "nowrap"
    | `ObjectBoundingBox -> "objectBoundingBox"
    | `OnLoad -> "onLoad"
    | `OnRequest -> "onRequest"
    | `OptimizeLegibility -> "optimizeLegibility"
    | `OptimizeQuality -> "optimizeQuality"
    | `OptimizeSpeed -> "optimizeSpeed"
    | `Origin -> "origin"
    | `Origin_when_cross_origin -> "origin-when-cross-origin"
    | `Other -> "other"
    | `Out -> "out"
    | `Over -> "over"
    | `Overlay -> "overlay"
    | `Overline -> "overline"
    | `Paced -> "paced"
    | `Pad -> "pad"
    | `Painted -> "painted"
    | `Perceptual -> "perceptual"
    | `Pre -> "pre"
    | `Pre_line -> "pre-line"
    | `Pre_wrap -> "pre-wrap"
    | `Preserve -> "preserve"
    | `R -> "r"
    | `Reflect -> "reflect"
    | `Relative_colorimetric -> "relative-colorimetric"
    | `Remove -> "remove"
    | `Repeat -> "repeat"
    | `Replace -> "replace"
    | `Right -> "right"
    | `Rotate -> "rotate"
    | `Round -> "round"
    | `Rtl -> "rtl"
    | `Same_origin -> "same-origin"
    | `Saturate -> "saturate"
    | `Saturation -> "saturation"
    | `Scale -> "scale"
    | `Screen -> "screen"
    | `Scroll -> "scroll"
    | `SkewX -> "skewX"
    | `SkewY -> "skewY"
    | `Soft_light -> "soft-light"
    | `Spacing -> "spacing"
    | `SpacingAndGlyphs -> "spacingAndGlyphs"
    | `Spline -> "spline"
    | `Square -> "square"
    | `SRGB -> "sRGB"
    | `Start -> "start"
    | `Stitch -> "stitch"
    | `Stretch -> "stretch"
    | `Strict_origin -> "strict-origin"
    | `Strict_origin_when_cross_origin -> "strict-origin-when-cross-origin"
    | `Stroke -> "stroke"
    | `StrokeWidth -> "stroke-width"
    | `Sum -> "sum"
    | `Sync -> "sync"
    | `Table -> "table"
    | `Terminal -> "terminal"
    | `Translate -> "translate"
    | `Turbulence -> "turbulence"
    | `Underline -> "underline"
    | `Unsafe_url -> "unsafe-url"
    | `Use_credentials -> "use-credentials"
    | `UserSpaceOnUse -> "userSpaceOnUse"
    | `V -> "v"
    | `Vertical_lr -> "vertical-lr"
    | `Vertical_rl -> "vertical-rl"
    | `Visible -> "visible"
    | `VisibleFill -> "visibleFill"
    | `VisiblePainted -> "visiblePainted"
    | `VisibleStroke -> "visibleStroke"
    | `WhenNotActive -> "whenNotActive"
    | `Wrap -> "wrap"
    | `XML -> "XML"
    | `Xor -> "xor"

  let string_of_bool = string_of_bool

  let unoption_string = function
    | Some x -> x
    | None -> ""

  let string_of_coords =
    list (fun (a, b) -> Printf.sprintf "%g, %g" a b)

  let string_of_dominant_baseline = function
    | `Auto -> "auto"
    | `Use_script -> "use-script"
    | `No_change -> "no-change"
    | `Reset_size -> "reset-size"
    | `Ideographic -> "ideographic"
    | `Alphabetic -> "alphabetic"
    | `Hanging -> "hanging"
    | `Mathematical -> "mathematical"
    | `Central -> "central"
    | `Middle -> "middle"
    | `Text_after_edge -> "text-after-edge"
    | `Text_before_edge -> "text-before-edge"
    | `Text_bottom -> "text-bottom"
    | `Text_top -> "text-top"
    | `Inherit -> "inherit"


  let string_of_in_value = function
    | `SourceGraphic -> "SourceGraphic"
    | `SourceAlpha -> "SourceAlpha"
    | `BackgroundImage -> "BackgroundImage"
    | `BackgroundAlpha -> "BackgroundAlpha"
    | `FillPaint -> "FillPaint"
    | `StrokePaint -> "StrokePaint"
    | `Ref _svg -> _svg

  let string_of_int = string_of_int

  let string_of_length = Unit.string_of_length

  let string_of_lengths = list string_of_length

  let string_of_number = Xml_print.string_of_number

  let string_of_percentage x = (string_of_number x) ^ "%"

  let string_of_fourfloats (a, b, c, d) =
    Printf.sprintf "%s %s %s %s"
      (string_of_number a) (string_of_number b) (string_of_number c) (string_of_number d)

  let string_of_number_optional_number = function
    | x, Some y -> Printf.sprintf "%g, %g" x y
    | x, None -> Printf.sprintf "%g" x

  let string_of_numbers = list string_of_number

  let string_of_numbers_semicolon = list ~sep:"; " string_of_number

  let string_of_offset = function
    | `Number x -> string_of_number x
    | `Percentage x -> string_of_percentage x

  let string_of_orient = function
    | None -> "auto"
    | Some __svg -> string_of_angle __svg

  let string_of_paint = string_of_paint

  let string_of_fill_rule = string_of_fill_rule

  let string_of_strokedasharray = function
    | [] -> "none"
    | l -> list string_of_length l

  let string_of_transform = function
    | `Matrix (a, b, c, d, e, f) ->
      Printf.sprintf "matrix(%g %g %g %g %g %g)" a b c d e f
    | `Translate x ->
      Printf.sprintf "translate(%s)"
        (string_of_number_optional_number x)
    | `Scale x ->
      Printf.sprintf "scale(%s)" (string_of_number_optional_number x)
    | `Rotate ((angle, x)) ->
      Printf.sprintf "rotate(%s %s)" (string_of_angle angle)
        (match x with
         | Some ((x, y)) -> Printf.sprintf "%g %g" x y
         | None -> "")
    | `SkewX angle ->
      Printf.sprintf "skewX(%s)" (string_of_angle angle)
    | `SkewY angle ->
      Printf.sprintf "skewY(%s)" (string_of_angle angle)

  let string_of_transforms l =
    String.concat " " (List.map string_of_transform l)

end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = ('a -> 'b)) =
  Make_with_wrapped_functions(Xml)(Wrapped_functions(Xml))
