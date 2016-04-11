(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
 * Copyright (C) 2007 Gabriel Kerneis
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)
(*
  Syntax extension for xml

*)
open Xhtmlparser;
open Camlp4.PreCast;

module Parser5 = Xhtmlparser.Make(Syntax)(struct
  value xml_encodedpcdata _loc = <:expr< Html.Xml.encodedpcdata >>;
  value xml_pcdata _loc = <:expr< Html.Xml.pcdata >>;
  value xml_comment _loc = <:expr< Html.Xml.comment >>;
  value xml_node _loc = <:expr< Html.Xml.node >>;
  value xml_string_attrib _loc = <:expr< Html.Xml.string_attrib >>;
  value tot _loc = <:expr< Html.tot >>;
  value toeltl _loc = <:expr< Html.toeltl >>;
  value to_xmlattribs _loc = <:expr< Html.to_xmlattribs >>;
  value to_attrib _loc = <:expr< Html.to_attrib >>;
  value make_type _loc tag =
    let tag = match String.lowercase tag with
      [ "object" -> "object__"
      | "option" -> "selectoption"
      | "audio" -> "audio_"
      | "video" -> "video_"
      | "canvas" -> "canvas_"
      | "map" -> "map_"
      | "ins" -> "ins_"
      | "ins_" -> "del_"
      | x -> x ] in
    match tag with
      [ "a" -> <:ctyp< Html.elt [> Html_types.a 'a ] >>
      | tag -> <:ctyp< Html.elt [> Html_types.$lid:tag$ ] >> ];
  value make_content_type _loc tag =
    <:ctyp< Html.elt [< Html_types.$lid:String.lowercase tag^"_content"$] >>;
  value make_attrib_type _loc tag =
      let tag = match String.lowercase tag with
      [ "button_type" -> "Button_Type"
      | "command_type" -> "Command_Type"
      | "float_value" -> "Float_Value"
      | "for_list" -> "For_List"
      | "input_max" -> "Input_Max"
      | "input_min" -> "Input_Min"
      | "input_type" -> "Input_Type"
      | "int_value" -> "Int_Value"
      | "menu_type" -> "Menu_Type"
      | "readonly" -> "ReadOnly"
      | "style_attr" -> "Style_Attr"
      | "text_value" -> "Text_Value"
      | "xml_lang" -> "XML_lang"
      | "onabort" -> "OnAbort"
      | "onafterprint" -> "OnAfterPrint"
      | "onbeforeprint" -> "OnBeforePrint"
      | "onblur" -> "OnBlur"
      | "oncanplay" -> "OnCanPlay"
      | "oncanplaythrough" -> "OnCanPlayThrough"
      | "onchange" -> "OnChange"
      | "onclick" -> "OnClick"
      | "oncontextmenu" -> "OnContextMenu"
      | "ondblclick" -> "OnDblClick"
      | "ondrag" -> "OnDrag"
      | "ondragend" -> "OnDragEnd"
      | "ondragenter" -> "OnDragEnter"
      | "ondragleave" -> "OnDragLeave"
      | "ondragover" -> "OnDragOver"
      | "ondragstart" -> "OnDragStart"
      | "ondrop" -> "OnDrop"
      | "ondurationchange" -> "OnDurationChange"
      | "onemptied" -> "OnEmptied"
      | "onended" -> "OnEnded"
      | "onerror" -> "OnError"
      | "onfocus" -> "OnFocus"
      | "onformchange" -> "OnFormChange"
      | "onforminput" -> "OnFormInput"
      | "onhashchange" -> "OnHashChange"
      | "oninput" -> "OnInput"
      | "oninvalid" -> "OnInvalid"
      | "onkeydown" -> "OnKeyDown"
      | "onkeypress" -> "OnKeyPress"
      | "onkeyup" -> "OnKeyUp"
      | "onload" -> "OnLoad"
      | "onloadstart" -> "OnLoadStart"
      | "onloadeddata" -> "OnLoadedData"
      | "onloadedmetadata" -> "OnLoadedMetaData"
      | "onmessage" -> "OnMessage"
      | "onmousedown" -> "OnMouseDown"
      | "onmousemove" -> "OnMouseMove"
      | "onmouseout" -> "OnMouseOut"
      | "onmouseover" -> "OnMouseOver"
      | "onmouseup" -> "OnMouseUp"
      | "onmousewheel" -> "OnMouseWheel"
      | "onoffline" -> "OnOffLine"
      | "ononline" -> "OnOnLine"
      | "onpagehide" -> "OnPageHide"
      | "onpageshow" -> "OnPageShow"
      | "onpause" -> "OnPause"
      | "onplay" -> "OnPlay"
      | "onplaying" -> "OnPlaying"
      | "onpopstate" -> "OnPopState"
      | "onprogress" -> "OnProgress"
      | "onratechange" -> "OnRateChange"
      | "onreadystatechange" -> "OnReadyStateChange"
      | "onredo" -> "OnRedo"
      | "onresize" -> "OnResize"
      | "onscroll" -> "OnScroll"
      | "onseeked" -> "OnSeeked"
      | "onseeking" -> "OnSeeking"
      | "onselect" -> "OnSelect"
      | "onshow" -> "OnShow"
      | "onstalled" -> "OnStalled"
      | "onstorage" -> "OnStorage"
      | "onsubmit" -> "OnSubmit"
      | "onsuspend" -> "OnSuspend"
      | "ontimeupdate" -> "OnTimeUpdate"
      | "onundo" -> "OnUndo"
      | "onunload" -> "OnUnload"
      | "onvolumechange" -> "OnVolumeChange"
      | "onwaiting" -> "OnWaiting"
      | "onebeforeunload" -> "OneBeforeUnload"
      | tag -> String.capitalize tag ] in
    <:ctyp< Html.attrib [> `$uid:tag$ ] >>;
  value make_attribs_type _loc tag =
    match String.lowercase tag with
    [ "img" -> <:ctyp< Html.attrib [< `Alt | `Src | Html_types.img_attrib] >>
    | tag -> <:ctyp< Html.attrib [< Html_types.$lid:tag^"_attrib"$] >>
    ] ;
end);


module ParserSvg = Xhtmlparser.Make(Syntax)(struct
  value xml_encodedpcdata _loc = <:expr< Svg.Xml.encodedpcdata >>;
  value xml_pcdata _loc = <:expr< Svg.Xml.pcdata >>;
  value xml_comment _loc = <:expr< Svg.Xml.comment >>;
  value xml_node _loc = <:expr< Svg.Xml.node >>;
  value xml_string_attrib _loc = <:expr< Svg.Xml.string_attrib >>;
  value tot _loc = <:expr< Svg.tot >>;
  value toeltl _loc = <:expr< Svg.toeltl >>;
  value to_xmlattribs _loc = <:expr< Svg.to_xmlattribs >>;
  value to_attrib _loc = <:expr< Svg.to_attrib >>;
  value make_type _loc tag =
    <:ctyp< Svg.elt [> `$uid:String.capitalize tag$ ] >>;
  value make_content_type _loc tag =
    <:ctyp< Svg.elt [< Svg_types.$lid:String.lowercase tag^"_content"$] >>;
  value make_attrib_type _loc tag =
    let tag = match String.lowercase tag with
    [ "accent-height" -> "AccentHeight"
    | "alignment-baseline" -> "Alignment_Baseline"
    | "altglyph" -> "AltGlyph"
    | "animatecolor" -> "AnimateColor"
    | "arabic-form" -> "ArabicForm"
    | "attributename" -> "AttributeName"
    | "attributetype" -> "AttributeType"
    | "basefrequency" -> "BaseFrequency"
    | "baseline-shift" -> "Baseline_Shift"
    | "baseprofile" -> "BaseProfile"
    | "calcmode" -> "CalcMode"
    | "cap-height" -> "CapHeight"
    | "clip-path" -> "Clip_Path"
    | "clippathunits" -> "ClipPathUnits"
    | "clip-rule" -> "Clip_Rule"
    | "color-interpolation" -> "Color_Interpolation"
    | "contentscripttype" -> "ContentScriptType"
    | "contentstyletype" -> "ContentStyleType"
    | "diffuseconstant" -> "DiffuseConstant"
    | "edgemode" -> "EdgeMode"
    | "externalresourcesrequired" -> "ExternalResourcesRequired"
    | "filterres" -> "FilterRes"
    | "filterunits" -> "FilterUnits"
    | "flood-color" -> "Flood_Color"
    | "flood-opacity" -> "Flood_Opacity"
    | "font-family" -> "Font_Family"
    | "font-size" -> "Font_Size"
    | "font-size-adjust" -> "Font_Size_Adjust"
    | "font-stretch" -> "Font_Stretch"
    | "font-style" -> "Font_Style"
    | "font-variant" -> "Font_Variant"
    | "font-weight" -> "Font_Weight"
    | "glyph-name" -> "GlyphName"
    | "glyph_orientation_horizontal" -> "Glyph_Orientation_Horizontal"
    | "glyph_orientation_vertical" -> "Glyph_Orientation_Vertical"
    | "glyphref" -> "GlyphRef"
    | "gradienttransform" -> "GradientTransform"
    | "gradientunits" -> "GradientUnits"
    | "horiz-adv-x" -> "HorizAdvX"
    | "horiz-origin-x" -> "HorizOriginX"
    | "horiz-origin-y" -> "HorizOriginY"
    | "image_rendering" -> "Image_Rendering"
    | "kernelmatrix" -> "KernelMatrix"
    | "kernelunitlength" -> "KernelUnitLength"
    | "keypoints" -> "KeyPoints"
    | "keysplines" -> "KeySplines"
    | "keytimes" -> "KeyTimes"
    | "lengthadjust" -> "LengthAdjust"
    | "letter_spacing" -> "Letter_Spacing"
    | "lighting_color" -> "Lighting_Color"
    | "limitingconeangle" -> "LimitingConeAngle"
    | "marker_end" -> "Marker_End"
    | "markerheight" -> "MarkerHeight"
    | "marker_mid" -> "Marker_Mid"
    | "marker_start" -> "Marker_Start"
    | "markerunits" -> "MarkerUnits"
    | "markerwidth" -> "MarkerWidth"
    | "maskcontentunits" -> "MaskContentUnits"
    | "maskunits" -> "MaskUnits"
    | "missingglyph" -> "MissingGlyph"
    | "numoctaves" -> "NumOctaves"
    | "offset-transfer" -> "Offset_transfer"
    | "onabort" -> "OnAbort"
    | "onactivate" -> "OnActivate"
    | "onbegin" -> "OnBegin"
    | "onclick" -> "OnClick"
    | "onend" -> "OnEnd"
    | "onerror" -> "OnError"
    | "onfocusin" -> "OnFocusIn"
    | "onfocusout" -> "OnFocusOut"
    | "onload" -> "OnLoad"
    | "onmousedown" -> "OnMouseDown"
    | "onmouseout" -> "OnMouseOut"
    | "onmouseover" -> "OnMouseOver"
    | "onmouseup" -> "OnMouseUp"
    | "onrepeat" -> "OnRepeat"
    | "onresize" -> "OnResize"
    | "onscroll" -> "OnScroll"
    | "onunload" -> "OnUnload"
    | "onzoom" -> "OnZoom"
    | "overline-position" -> "OverlinePosition"
    | "overline-thickness" -> "OverlineThickness"
    | "pathlength" -> "PathLength"
    | "patterncontentunits" -> "PatternContentUnits"
    | "patterntransform" -> "PatternTransform"
    | "patternunits" -> "PatternUnits"
    | "pointer-events" -> "Pointer_Events"
    | "pointsatx" -> "PointsAtX"
    | "pointsaty" -> "PointsAtY"
    | "pointsatz" -> "PointsAtZ"
    | "preservealpha" -> "PreserveAlpha"
    | "preserveaspectradio" -> "PreserveAspectRadio"
    | "preserveaspectratio" -> "PreserveAspectRatio"
    | "primitiveunits" -> "PrimitiveUnits"
    | "refx" -> "RefX"
    | "refy" -> "RefY"
    | "rendering_intent" -> "Rendering_Intent"
    | "repeatcount" -> "RepeatCount"
    | "repeatdur" -> "RepeatDur"
    | "requiredextensions" -> "RequiredExtensions"
    | "requiredfeatures" -> "RequiredFeatures"
    | "shape-rendering" -> "Shape_Rendering"
    | "specularconstant" -> "SpecularConstant"
    | "specularexponent" -> "SpecularExponent"
    | "spreadmethod" -> "SpreadMethod"
    | "stddeviation" -> "StdDeviation"
    | "stitchtiles" -> "StitchTiles"
    | "stop-color" -> "Stop_Color"
    | "stop-opacity" -> "Stop_Opacity"
    | "strikethrough-position" -> "StrikethroughPosition"
    | "strikethrough-thickness" -> "StrikethroughThickness"
    | "stroke-dasharray" -> "Stroke_Dasharray"
    | "stroke-dashoffset" -> "Stroke_Dashoffset"
    | "stroke-linecap" -> "Stroke_Linecap"
    | "stroke-linejoin" -> "Stroke_Linejoin"
    | "stroke-miterlimit" -> "Stroke_Miterlimit"
    | "stroke-opacity" -> "Stroke_Opacity"
    | "stroke-width" -> "Stroke_Width"
    | "surfacescale" -> "SurfaceScale"
    | "systemlanguage" -> "SystemLanguage"
    | "tablevalues" -> "TableValues"
    | "targetx" -> "TargetX"
    | "targety" -> "TargetY"
    | "text-anchor" -> "Text_Anchor"
    | "text-decoration" -> "Text_Decoration"
    | "textlength" -> "TextLength"
    | "text-rendering" -> "Text_Rendering"
    | "underline-position" -> "Underline_Position"
    | "underline-thickness" -> "Underline_Thickness"
    | "unicode_bidi" -> "Unicode_Bidi"
    | "unicode-range" -> "UnicodeRange"
    | "units-per-em" -> "UnitsPerEm"
    | "v-alphabetic" -> "VAlphabetic"
    | "vert-adv-y" -> "VertAdvY"
    | "vert-origin-x" -> "VertOriginX"
    | "vert-origin-y" -> "VertOriginY"
    | "v-hanging" -> "VHanging"
    | "v-ideographic" -> "VIdeographic"
    | "viewbox" -> "ViewBox"
    | "viewtarget" -> "ViewTarget"
    | "v-mathematical" -> "VMathematical"
    | "word-spacing" -> "Word_Spacing"
    | "writing_mode" -> "Writing_Mode"
    | "xchannelselector" -> "XChannelSelector"
    | "ychannelselector" -> "YChannelSelector"
    | "zoomandpan" -> "ZoomAndPan"
    | "zoomandplan" -> "ZoomAndPlan"
    | tag -> String.capitalize tag ] in
    <:ctyp< Svg.attrib [> `$uid:tag$ ] >>;
  value make_attribs_type _loc tag =
    <:ctyp< Svg.attrib [< Svg_types.$lid:String.lowercase tag^"_attr"$] >>;
end);

do {
  Syntax.Quotation.add "html" Syntax.Quotation.DynAst.expr_tag Parser5.xml_exp ;
  Syntax.Quotation.add "htmllist" Syntax.Quotation.DynAst.expr_tag
        Parser5.xml_expl;
  Syntax.Quotation.default.val := "html";
  Syntax.Quotation.add "svg" Syntax.Quotation.DynAst.expr_tag ParserSvg.xml_exp ;
  Syntax.Quotation.add "svglist" Syntax.Quotation.DynAst.expr_tag
        ParserSvg.xml_expl
};
