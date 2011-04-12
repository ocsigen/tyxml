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

module Parser = Xhtmlparser.Make(Syntax)(struct
  value tot loc = <:expr< XHTML.M.tot >>;
  value toeltl loc = <:expr< XHTML.M.toeltl >>;
  value to_xmlattribs loc = <:expr< XHTML.M.to_xmlattribs >>;
  value to_attrib loc = <:expr< XHTML.M.to_attrib >>;
  value make_type loc tag =
    <:ctyp< XHTML.M.elt [> `$uid:String.capitalize tag$ ] >>;
  value make_content_type loc tag =
    <:ctyp< XHTML.M.elt [< XHTML_types.$lid:String.lowercase tag^"_content"$] >>;
  value make_attrib_type loc tag =
      let tag = match String.lowercase tag with
      [ "button_type" -> "Button_Type"
      | "fs_cols" -> "FS_Cols"
      | "fs_rows" -> "FS_Rows"
      | "input_type" -> "Input_Type"
      | "onblur" -> "OnBlur"
      | "onchange" -> "OnChange"
      | "onclick" -> "OnClick"
      | "ondblclick" -> "OnDblClick"
      | "onfocus" -> "OnFocus"
      | "onkeydown" -> "OnKeyDown"
      | "onkeypress" -> "OnKeyPress"
      | "onkeyup" -> "OnKeyUp"
      | "onload" -> "OnLoad"
      | "onmousedown" -> "OnMouseDown"
      | "onmousemove" -> "OnMouseMove"
      | "onmouseout" -> "OnMouseOut"
      | "onmouseover" -> "OnMouseOver"
      | "onmouseup" -> "OnMouseUp"
      | "onreset" -> "OnReset"
      | "onselect" -> "OnSelect"
      | "onsubmit" -> "OnSubmit"
      | "onunload" -> "OnUnload"
      | "style_attr" -> "Style_Attr"
      | "value_type" -> "Value_Type"
      | "xml_lang" -> "XML_lang"
      | "xml_space" -> "XML_space"
      | "xmlns" -> "XMLns"
      | tag -> String.capitalize tag ] in
    <:ctyp< XHTML.M.attrib [> `$uid:tag$ ] >>;
  value make_attribs_type loc tag =
    <:ctyp< XHTML.M.attrib [< XHTML_types.$lid:String.lowercase tag^"_attrib"$] >>;
end);

module Parser5 = Xhtmlparser.Make(Syntax)(struct
  value tot loc = <:expr< HTML5.M.tot >>;
  value toeltl loc = <:expr< HTML5.M.toeltl >>;
  value to_xmlattribs loc = <:expr< HTML5.M.to_xmlattribs >>;
  value to_attrib loc = <:expr< HTML5.M.to_attrib >>;
  value make_type loc tag =
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
      [ "a" -> <:ctyp< HTML5.M.elt [> HTML5_types.a 'a ] >>
      | tag -> <:ctyp< HTML5.M.elt [> HTML5_types.$lid:tag$ ] >> ];
  value make_content_type loc tag =
    <:ctyp< HTML5.M.elt [< HTML5_types.$lid:String.lowercase tag^"_content"$] >>;
  value make_attrib_type loc tag =
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
    <:ctyp< HTML5.M.attrib [> `$uid:tag$ ] >>;
  value make_attribs_type loc tag =
    <:ctyp< HTML5.M.attrib [< HTML5_types.$lid:String.lowercase tag^"_attrib"$] >>;
end);


module ParserSVG = Xhtmlparser.Make(Syntax)(struct
  value tot loc = <:expr< SVG.M.tot >>;
  value toeltl loc = <:expr< SVG.M.toeltl >>;
  value to_xmlattribs loc = <:expr< SVG.M.to_xmlattribs >>;
  value to_attrib loc = <:expr< SVG.M.to_attrib >>;
  value make_type loc tag =
    <:ctyp< SVG.M.elt [> `$uid:String.capitalize tag$ ] >>;
  value make_content_type loc tag =
    <:ctyp< SVG.M.elt [< SVG_types.$lid:String.lowercase tag^"_content"$] >>;
  value make_attrib_type loc tag =
    let tag = match String.lowercase tag with
    [ "accent___height" -> "Accent___Height"
    | "alignement_baseline" -> "Alignement_Baseline"
    | "altglyph" -> "AltGlyph"
    | "animatecolor" -> "AnimateColor"
    | "arabic___form" -> "Arabic___Form"
    | "attributename" -> "AttributeName"
    | "attributetype" -> "AttributeType"
    | "basefrequency" -> "BaseFrequency"
    | "baseline_shift" -> "Baseline_Shift"
    | "baseprofile" -> "BaseProfile"
    | "calcmode" -> "CalcMode"
    | "cap___height" -> "Cap___Height"
    | "clip_path" -> "Clip_Path"
    | "clippathunits" -> "ClipPathUnits"
    | "clip_rule" -> "Clip_Rule"
    | "color_interpolation" -> "Color_Interpolation"
    | "contentscripttype" -> "ContentScriptType"
    | "contentstyletype" -> "ContentStyleType"
    | "diffuseconstant" -> "DiffuseConstant"
    | "edgemode" -> "EdgeMode"
    | "externalresourcesrequired" -> "ExternalResourcesRequired"
    | "filterres" -> "FilterRes"
    | "filterunits" -> "FilterUnits"
    | "flood_color" -> "Flood_Color"
    | "flood_opacity" -> "Flood_Opacity"
    | "font_face" -> "Font_Face"
    | "font_family" -> "Font_Family"
    | "font___family" -> "Font___Family"
    | "font_size" -> "Font_Size"
    | "font___size" -> "Font___Size"
    | "font_size_adjust" -> "Font_Size_Adjust"
    | "font_stretch" -> "Font_Stretch"
    | "font___stretch" -> "Font___Stretch"
    | "font_style" -> "Font_Style"
    | "font___style" -> "Font___Style"
    | "font_variant" -> "Font_Variant"
    | "font___variant" -> "Font___Variant"
    | "font_weight" -> "Font_Weight"
    | "font___weight" -> "Font___Weight"
    | "glyph___name" -> "Glyph___Name"
    | "glyph_orientation_horizontal" -> "Glyph_Orientation_Horizontal"
    | "glyph_orientation_vertical" -> "Glyph_Orientation_Vertical"
    | "glyphref" -> "GlyphRef"
    | "gradienttransform" -> "GradientTransform"
    | "gradientunits" -> "GradientUnits"
    | "horiz___adv___x" -> "Horiz___Adv___X"
    | "horiz___origin___x" -> "Horiz___Origin___X"
    | "horiz___origin___y" -> "Horiz___Origin___Y"
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
    | "offset__transfer" -> "Offset__transfer"
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
    | "overline___position" -> "Overline___Position"
    | "overline___thickness" -> "Overline___Thickness"
    | "pathlength" -> "PathLength"
    | "patterncontentunits" -> "PatternContentUnits"
    | "patterntransform" -> "PatternTransform"
    | "patternunits" -> "PatternUnits"
    | "pointer_events" -> "Pointer_Events"
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
    | "shape_rendering" -> "Shape_Rendering"
    | "specularconstant" -> "SpecularConstant"
    | "specularexponent" -> "SpecularExponent"
    | "spreadmethod" -> "SpreadMethod"
    | "stddeviation" -> "StdDeviation"
    | "stitchtiles" -> "StitchTiles"
    | "stop_color" -> "Stop_Color"
    | "stop_opacity" -> "Stop_Opacity"
    | "strikethrough___position" -> "Strikethrough___Position"
    | "strikethrough___thickness" -> "Strikethrough___Thickness"
    | "stroke_dasharray" -> "Stroke_Dasharray"
    | "stroke_dashoffset" -> "Stroke_Dashoffset"
    | "stroke_linecap" -> "Stroke_Linecap"
    | "stroke_linejoin" -> "Stroke_Linejoin"
    | "stroke_miterlimit" -> "Stroke_Miterlimit"
    | "stroke_opacity" -> "Stroke_Opacity"
    | "stroke_width" -> "Stroke_Width"
    | "surfacescale" -> "SurfaceScale"
    | "systemlanguage" -> "SystemLanguage"
    | "tablevalues" -> "TableValues"
    | "targetx" -> "TargetX"
    | "targety" -> "TargetY"
    | "text_anchor" -> "Text_Anchor"
    | "text_decoration" -> "Text_Decoration"
    | "textlength" -> "TextLength"
    | "text_rendering" -> "Text_Rendering"
    | "underline___position" -> "Underline___Position"
    | "underline___thickness" -> "Underline___Thickness"
    | "unicode_bidi" -> "Unicode_Bidi"
    | "unicode___range" -> "Unicode___Range"
    | "units___per___em" -> "Units___Per___Em"
    | "v___alphabetic" -> "V___Alphabetic"
    | "vert___adv___y" -> "Vert___Adv___Y"
    | "vert___origin___x" -> "Vert___Origin___X"
    | "vert___origin___y" -> "Vert___Origin___Y"
    | "v___hanging" -> "V___Hanging"
    | "v___ideographic" -> "V___Ideographic"
    | "viewbox" -> "ViewBox"
    | "viewtarget" -> "ViewTarget"
    | "v___mathematical" -> "V___Mathematical"
    | "word_spacing" -> "Word_Spacing"
    | "writing_mode" -> "Writing_Mode"
    | "xchannelselector" -> "XChannelSelector"
    | "x___height" -> "X___Height"
    | "ychannelselector" -> "YChannelSelector"
    | "zoomandpan" -> "ZoomAndPan"
    | "zoomandplan" -> "ZoomAndPlan"
    | tag -> String.capitalize tag ] in
    <:ctyp< SVG.M.attrib [> `$uid:tag$ ] >>;
  value make_attribs_type loc tag =
    <:ctyp< SVG.M.attrib [< SVG_types.$lid:String.lowercase tag^"_attrib"$] >>;
end);

do {
  Syntax.Quotation.add "xhtml" Syntax.Quotation.DynAst.expr_tag Parser.xml_exp ;
  Syntax.Quotation.add "xhtmllist" Syntax.Quotation.DynAst.expr_tag
        Parser.xml_expl;
  Syntax.Quotation.add "html5" Syntax.Quotation.DynAst.expr_tag Parser5.xml_exp ;
  Syntax.Quotation.add "html5list" Syntax.Quotation.DynAst.expr_tag
        Parser5.xml_expl;
  Syntax.Quotation.default.val := "html5";
  Syntax.Quotation.add "svg" Syntax.Quotation.DynAst.expr_tag ParserSVG.xml_exp ;
  Syntax.Quotation.add "svglist" Syntax.Quotation.DynAst.expr_tag
        ParserSVG.xml_expl
};

