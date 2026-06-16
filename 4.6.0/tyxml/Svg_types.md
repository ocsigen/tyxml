
# Module `Svg_types`

SVG types with variants, goes with [`Svg_sigs.T`](./Svg_sigs-module-type-T.md).

This module defines basic data types for data, attributes and element occurring in SVG documents. It is based on the specification available at http://www.w3.org/TR/SVG/.

This module is experimental, it may lack of some attributes, and the interface is very low level and do not take deeply into account the needs of SVG elements.


## Categories of elements and attributes

This part defines the categories of elements and attributes


### Elements

```ocaml
type animation_element = [ 
  | `AnimateColor
  | `AnimateMotion
  | `AnimateTransform
  | `Animate
  | `Set
 ]
```
```ocaml
type descriptive_element = [ 
  | `Desc
  | `Metadata
  | `Title
 ]
```
```ocaml
type basic_shape_element = [ 
  | `Circle
  | `Ellipse
  | `Line
  | `Polygon
  | `Polyline
  | `Rect
 ]
```
```ocaml
type container_element = [ 
  | `A
  | `Defs
  | `Glyph
  | `G
  | `Marker
  | `Mask
  | `MissingGlyph
  | `Pattern
  | `Svg
  | `Switch
  | `Symbol
 ]
```
```ocaml
type filter_primitive_element = [ 
  | `FeBlend
  | `FeColorMatrix
  | `FeComponentTransfer
  | `FeComposite
  | `FeConvolveMatrix
  | `FeDiffuseLighting
  | `FeDisplacementMap
  | `FeFlood
  | `FeGaussianBlur
  | `FeImage
  | `FeMerge
  | `FeMorphology
  | `FeOffset
  | `FeSpecularLighting
  | `FeTile
  | `FeTurbulence
 ]
```
```ocaml
type light_source_element = [ 
  | `FeDistantLight
  | `FePointLight
  | `FeSpotLight
 ]
```
```ocaml
type shape_element = [ 
  | `Circle
  | `Ellipse
  | `Line
  | `Path
  | `Polyline
  | `Polygon
  | `Rect
 ]
```
```ocaml
type structural_element = [ 
  | `Defs
  | `G
  | `Svg
  | `Symbol
  | `Use
 ]
```
```ocaml
type text_content_element = [ 
  | `AltGlyph
  | `TextPath
  | `Text
  | `Tref
  | `Tspan
 ]
```
```ocaml
type text_content_child_element = [ 
  | `AltGlyph
  | `TextPath
  | `Tref
  | `Tspan
 ]
```
```ocaml
type gradient_element = [ 
  | `Lineargradient
  | `Radialgradient
 ]
```
```ocaml
type graphics_element = [ 
  | `Circle
  | `Ellipse
  | `Image
  | `Line
  | `Path
  | `Polygon
  | `Polyline
  | `Rect
  | `Text
  | `Use
 ]
```
```ocaml
type graphics_ref_element = [ 
  | `Image
  | `Use
 ]
```

### Attributes

```ocaml
type conditional_processing_attr = [ 
  | `RequiredExtensions
  | `RequiredFeatures
  | `SystemLanguage
 ]
```
```ocaml
type core_attr = [ 
  | `Id
  | `Xml_base
  | `Xml_lang
  | `Xml_space
  | `User_data
 ]
```
```ocaml
type transfer_attr = [ 
  | `Type_transfert
  | `TableValues
  | `Slope
  | `Intercept
  | `Amplitude
  | `Exponent
  | `Offset_transfer
 ]
```
```ocaml
type document_event_attr = [ 
  | `OnAbort
  | `OnError
  | `OnResize
  | `OnScroll
  | `OnUnload
  | `OnZoom
 ]
```
```ocaml
type filter_primitive_attr = [ 
  | `Height
  | `Result
  | `Width
  | `X
  | `Y
 ]
```
```ocaml
type animation_event_attr = [ 
  | `OnBegin
  | `OnEnd
  | `OnRepeat
  | `OnLoad
 ]
```
```ocaml
type animation_attr_target_attr = [ 
  | `AttributeType
  | `AttributeName
 ]
```
```ocaml
type animation_timing_attr = [ 
  | `Begin
  | `Dur
  | `End
  | `Min
  | `Max
  | `Restart
  | `RepeatCount
  | `RepeatDur
  | `Fill_Animation
 ]
```
```ocaml
type animation_value_attr = [ 
  | `CalcMode
  | `Valuesanim
  | `KeyTimes
  | `KeySplines
  | `From
  | `To
  | `By
 ]
```
```ocaml
type animation_addition_attr = [ 
  | `Additive
  | `Accumulate
 ]
```
```ocaml
type presentation_attr = [ 
  | `Alignment_Baseline
  | `Baseline_Shift
  | `Clip
  | `Clip_Path
  | `Clip_Rule
  | `Color
  | `Color_Interpolation
  | `Color_interpolation_filters
  | `Color_profile
  | `Color_rendering
  | `Cursor
  | `Direction
  | `Display
  | `Dominant_Baseline
  | `Enable_background
  | `Fill
  | `Fill_opacity
  | `Fill_rule
  | `Filter
  | `Flood_Color
  | `Flood_Opacity
  | `Font_Family
  | `Font_Size
  | `Font_Size_Adjust
  | `Font_Stretch
  | `Font_Style
  | `Font_Variant
  | `Font_Weight
  | `Glyph_Orientation_Horizontal
  | `Glyph_Orientation_Vertical
  | `Image_Rendering
  | `Kerning
  | `Letter_Spacing
  | `Lighting_Color
  | `Marker_End
  | `Marker_Mid
  | `Marker_Start
  | `Mask
  | `Opacity
  | `Overflow
  | `Pointer_Events
  | `Shape_Rendering
  | `Stop_Color
  | `Stop_Opacity
  | `Stroke
  | `Stroke_Dasharray
  | `Stroke_Dashoffset
  | `Stroke_Linecap
  | `Stroke_Linejoin
  | `Stroke_Miterlimit
  | `Stroke_Opacity
  | `Stroke_Width
  | `Text_Anchor
  | `Text_Decoration
  | `Text_Rendering
  | `Unicode_Bidi
  | `Visibility
  | `Word_Spacing
  | `Writing_Mode
 ]
```
```ocaml
type graphical_event_attr = [ 
  | `OnActivate
  | `OnClick
  | `OnFocusIn
  | `OnFocusOut
  | `OnLoad
  | `OnMouseDown
  | `OnMouseMove
  | `OnMouseOut
  | `OnMouseOver
  | `OnMouseUp
 ]
```
```ocaml
type xlink_attr = [ 
  | `Xlink_href
  | `Xlink_type
  | `Xlink_role
  | `Xlink_arcrole
  | `Xlink_title
  | `Xlink_show
  | `Xlink_actuate
 ]
```

### Generic data types

```ocaml
type iri = string
```
An IRI reference is an Internationalized Resource Identifier with an optional fragment identifier, as defined in Internationalized Resource Identifiers `RFC3987`. An IRI reference serves as a reference to a resource or (with a fragment identifier) to a secondary resource. See References and the ‘defs’ element..


### Units

```ocaml
module Unit : sig ... end
```
SVG defines several units to measure time, length, angles.

```ocaml
type coord = Unit.length
```
```ocaml
type number = float
```
```ocaml
type number_optional_number = number * number option
```
```ocaml
type percentage = float
```
```ocaml
type strings = string list
```
```ocaml
type color = string
```
```ocaml
type icccolor = string
```
```ocaml
type paint_without_icc = [ 
  | `None
  | `CurrentColor
  | `Color of color * icccolor option
 ]
```
```ocaml
type paint = [ 
  | paint_without_icc
  | `Icc of iri * paint_without_icc option
 ]
```
```ocaml
type fill_rule = [ 
  | `Nonzero
  | `Evenodd
 ]
```
```ocaml
type transform = [ 
  | `Matrix of float * float * float * float * float * float
  | `Translate of float * float option
  | `Scale of float * float option
  | `Rotate of Unit.angle * (float * float) option
  | `SkewX of Unit.angle
  | `SkewY of Unit.angle
 ]
```
```ocaml
type spacestrings = string list
```
```ocaml
type commastrings = string list
```
```ocaml
type transforms = transform list
```
```ocaml
type fourfloats = float * float * float * float
```
```ocaml
type lengths = Unit.length list
```
```ocaml
type numbers = float list
```
```ocaml
type numbers_semicolon = float list
```
```ocaml
type coords = (float * float) list
```
```ocaml
type rotate = float list
```
```ocaml
type pcdata = [ 
  | `PCDATA
 ]
```
```ocaml
type txt = [ 
  | `PCDATA
 ]
```

## Element

```ocaml
type svg = [ 
  | `Svg
 ]
```
```ocaml
type svg_content = [ 
  | animation_element
  | descriptive_element
  | shape_element
  | structural_element
  | gradient_element
  | `A
  | `AltGlyphDef
  | `ClipPath
  | `Color_Profile
  | `Cursor
  | `Filter
  | `Font
  | `Font_Face
  | `ForeignObject
  | `Image
  | `Marker
  | `Mask
  | `Pattern
  | `Script
  | `Style
  | `Switch
  | `Text
  | `View
 ]
```
```ocaml
type svg_attr = [ 
  | conditional_processing_attr
  | core_attr
  | document_event_attr
  | graphical_event_attr
  | presentation_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `X
  | `Y
  | `Width
  | `Height
  | `ViewBox
  | `PreserveAspectRatio
  | `ZoomAndPan
  | `Version
  | `BaseProfile
  | `ContentScriptType
  | `ContentStyleType
  | `X
  | `Y
 ]
```
```ocaml
type g = [ 
  | `G
 ]
```
```ocaml
type g_content = [ 
  | animation_element
  | descriptive_element
  | shape_element
  | structural_element
  | gradient_element
  | `A
  | `AltGlyphDef
  | `ClipPath
  | `Color_Profile
  | `Cursor
  | `Filter
  | `Font
  | `Font_Face
  | `ForeignObject
  | `Image
  | `Marker
  | `Mask
  | `Pattern
  | `Script
  | `Style
  | `Switch
  | `Text
  | `View
 ]
```
```ocaml
type g_attr = [ 
  | conditional_processing_attr
  | core_attr
  | graphical_event_attr
  | presentation_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `Transform
 ]
```
```ocaml
type defs = [ 
  | `Defs
 ]
```
```ocaml
type defs_content = [ 
  | animation_element
  | descriptive_element
  | shape_element
  | structural_element
  | gradient_element
  | `A
  | `AltGlyphDef
  | `ClipPath
  | `Color_Profile
  | `Cursor
  | `Filter
  | `Font
  | `Font_Face
  | `ForeignObject
  | `Image
  | `Marker
  | `Mask
  | `Pattern
  | `Script
  | `Style
  | `Switch
  | `Text
  | `View
 ]
```
```ocaml
type defs_attr = [ 
  | conditional_processing_attr
  | core_attr
  | graphical_event_attr
  | presentation_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `Transform
 ]
```
```ocaml
type desc = [ 
  | `Desc
 ]
```
```ocaml
type desc_content = [ 
  | `PCDATA
 ]
```
```ocaml
type desc_attr = [ 
  | core_attr
  | `Class
  | `Style
 ]
```
```ocaml
type title = [ 
  | `Title
 ]
```
```ocaml
type title_content = [ 
  | `PCDATA
 ]
```
```ocaml
type title_attr = desc_attr
```
```ocaml
type symbol = [ 
  | `Symbol
 ]
```
```ocaml
type symbol_content = [ 
  | animation_element
  | descriptive_element
  | structural_element
  | gradient_element
  | `A
  | `AltGlyphDef
  | `ClipPath
  | `Color_Profile
  | `Cursor
  | `Filter
  | `Font
  | `Font_Face
  | `ForeignObject
  | `Image
  | `Marker
  | `Mask
  | `Pattern
  | `Script
  | `Style
  | `Switch
  | `Text
  | `View
 ]
```
```ocaml
type symbol_attr = [ 
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `PreserveAspectRatio
  | `ViewBox
 ]
```
```ocaml
type use = [ 
  | `Use
 ]
```
```ocaml
type use_content = [ 
  | animation_element
  | descriptive_element
 ]
```
```ocaml
type use_attr = [ 
  | core_attr
  | conditional_processing_attr
  | graphical_event_attr
  | presentation_attr
  | xlink_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `Transform
  | `X
  | `Y
  | `Width
  | `Height
  | `Xlink_href
 ]
```
```ocaml
type image = [ 
  | `Image
 ]
```
```ocaml
type image_content = [ 
  | animation_element
  | descriptive_element
 ]
```
```ocaml
type image_attr = [ 
  | core_attr
  | conditional_processing_attr
  | graphical_event_attr
  | xlink_attr
  | presentation_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `PreserveAspectRatio
  | `Transform
  | `X
  | `Y
  | `Width
  | `Height
  | `Xlink_href
 ]
```
```ocaml
type switch = [ 
  | `Switch
 ]
```
```ocaml
type switch_content = [ 
  | animation_element
  | descriptive_element
  | shape_element
  | `A
  | `ForeignObject
  | `G
  | `Image
  | `Svg
  | `Switch
  | `Text
  | `Use
 ]
```
```ocaml
type switch_attr = [ 
  | conditional_processing_attr
  | core_attr
  | graphical_event_attr
  | presentation_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `Transform
 ]
```
```ocaml
type style = [ 
  | `Style
 ]
```
```ocaml
type style_content = [ 
  | `PCDATA
 ]
```
```ocaml
type style_attr = [ 
  | core_attr
  | `Title
  | `Media
  | `Type
 ]
```
```ocaml
type path = [ 
  | `Path
 ]
```
```ocaml
type path_content = [ 
  | animation_element
  | descriptive_element
 ]
```
```ocaml
type path_attr = [ 
  | conditional_processing_attr
  | core_attr
  | graphical_event_attr
  | presentation_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `Transform
  | `D
  | `PathLength
 ]
```
```ocaml
type rect = [ 
  | `Rect
 ]
```
```ocaml
type rect_content = [ 
  | animation_element
  | descriptive_element
 ]
```
```ocaml
type rect_attr = [ 
  | conditional_processing_attr
  | core_attr
  | graphical_event_attr
  | presentation_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `Transform
  | `X
  | `Y
  | `Width
  | `Height
  | `Rx
  | `Ry
 ]
```
```ocaml
type circle = [ 
  | `Circle
 ]
```
```ocaml
type circle_content = [ 
  | animation_element
  | descriptive_element
 ]
```
```ocaml
type circle_attr = [ 
  | conditional_processing_attr
  | core_attr
  | graphical_event_attr
  | presentation_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `Transform
  | `R
  | `Cx
  | `Cy
 ]
```
```ocaml
type ellipse = [ 
  | `Ellipse
 ]
```
```ocaml
type ellipse_content = [ 
  | animation_element
  | descriptive_element
 ]
```
```ocaml
type ellipse_attr = [ 
  | conditional_processing_attr
  | core_attr
  | graphical_event_attr
  | presentation_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `Transform
  | `Rx
  | `Ry
  | `Cx
  | `Cy
 ]
```
```ocaml
type line = [ 
  | `Line
 ]
```
```ocaml
type line_content = [ 
  | animation_element
  | descriptive_element
 ]
```
```ocaml
type line_attr = [ 
  | conditional_processing_attr
  | core_attr
  | graphical_event_attr
  | presentation_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `Transform
  | `X1
  | `Y1
  | `X2
  | `Y2
 ]
```
```ocaml
type polyline = [ 
  | `Polyline
 ]
```
```ocaml
type polyline_content = [ 
  | animation_element
  | descriptive_element
 ]
```
```ocaml
type polyline_attr = [ 
  | conditional_processing_attr
  | core_attr
  | graphical_event_attr
  | presentation_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `Transform
  | `Points
 ]
```
```ocaml
type polygon = [ 
  | `Polygon
 ]
```
```ocaml
type polygon_content = [ 
  | animation_element
  | descriptive_element
 ]
```
```ocaml
type polygon_attr = [ 
  | conditional_processing_attr
  | core_attr
  | graphical_event_attr
  | presentation_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `Transform
  | `Points
 ]
```
```ocaml
type text = [ 
  | `Text
 ]
```
```ocaml
type text_content = [ 
  | animation_element
  | descriptive_element
  | text_content_child_element
  | `PCDATA
  | `A
 ]
```
```ocaml
type text_attr = [ 
  | conditional_processing_attr
  | core_attr
  | graphical_event_attr
  | presentation_attr
  | `Class
  | `Transform
  | `LengthAdjust
  | `X_list
  | `Y_list
  | `Dx_list
  | `Dy_list
  | `Rotate
  | `TextLength
 ]
```
```ocaml
type tspan = [ 
  | `Tspan
 ]
```
```ocaml
type tspan_content = [ 
  | descriptive_element
  | core_attr
  | `PCDATA
  | `A
  | `AltGlyph
  | `Animate
  | `AnimateColor
  | `Set
  | `Tref
  | `Tspan
 ]
```
```ocaml
type tspan_attr = [ 
  | core_attr
  | conditional_processing_attr
  | graphical_event_attr
  | presentation_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `X_list
  | `Y_list
  | `Dx_list
  | `Dy_list
  | `Rotate
  | `TextLength
  | `LengthAdjust
 ]
```
```ocaml
type tref = [ 
  | `Tref
 ]
```
```ocaml
type tref_content = [ 
  | descriptive_element
  | `Animate
  | `AnimateColor
  | `Set
 ]
```
```ocaml
type tref_attr = [ 
  | conditional_processing_attr
  | core_attr
  | graphical_event_attr
  | presentation_attr
  | xlink_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `Xlink_href
 ]
```
```ocaml
type textpath = [ 
  | `TextPath
 ]
```
```ocaml
type textpath_content = [ 
  | descriptive_element
  | `PCDATA
  | `A
  | `AltGlyph
  | `Animate
  | `AnimateColor
  | `Set
  | `Tref
  | `Tspan
 ]
```
```ocaml
type textpath_attr = [ 
  | conditional_processing_attr
  | core_attr
  | graphical_event_attr
  | presentation_attr
  | xlink_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `Xlink_href
  | `StartOffset
  | `Method
  | `Spacing
 ]
```
```ocaml
type altglyph = [ 
  | `AltGlyph
 ]
```
```ocaml
type altglyph_content = [ 
  | `PCDATA
 ]
```
```ocaml
type altglyph_attr = [ 
  | conditional_processing_attr
  | core_attr
  | graphical_event_attr
  | presentation_attr
  | xlink_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `X_list
  | `Y_list
  | `Dx_list
  | `Dy_list
  | `GlyphRef
  | `Format
  | `Rotate
  | `Xlink_href
 ]
```
```ocaml
type altglyphdef = [ 
  | `AltGlyphDef
 ]
```
```ocaml
type altglyphdef_attr = [ 
  | core_attr
 ]
```
```ocaml
type altglyphitem = [ 
  | `AltGlyphItem
 ]
```
```ocaml
type altglyphitem_content = [ 
  | `GlyphRef
 ]
```
```ocaml
type altglyphitem_attr = [ 
  | core_attr
 ]
```
```ocaml
type glyphref = [ 
  | `GlyphRef
 ]
```
```ocaml
type glyphref_attr = [ 
  | core_attr
  | presentation_attr
  | xlink_attr
  | `Class
  | `Style
  | `X
  | `Y
  | `Dx
  | `Dy
  | `GlyphRef
  | `Format
  | `Xlink_href
 ]
```
```ocaml
type marker = [ 
  | `Marker
 ]
```
```ocaml
type marker_content = [ 
  | animation_element
  | descriptive_element
  | shape_element
  | structural_element
  | gradient_element
  | `A
  | `AltGlyphDef
  | `ClipPath
  | `Color_Profile
  | `Cursor
  | `Filter
  | `Font
  | `Font_Face
  | `ForeignObject
  | `Image
  | `Marker
  | `Mask
  | `Pattern
  | `Script
  | `Style
  | `Switch
  | `Text
  | `View
 ]
```
```ocaml
type marker_attr = [ 
  | core_attr
  | presentation_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `ViewBox
  | `PreserveAspectRatio
  | `RefX
  | `RefY
  | `MarkerUnits
  | `MarkerWidth
  | `MarkerHeight
  | `Orient
 ]
```
```ocaml
type colorprofile = [ 
  | `Color_Profile
 ]
```
```ocaml
type colorprofile_content = [ 
  | descriptive_element
 ]
```
```ocaml
type colorprofile_attr = [ 
  | core_attr
  | xlink_attr
  | `Local
  | `Name
  | `Rendering_Intent
  | `Xlink_href
 ]
```
```ocaml
type lineargradient = [ 
  | `Lineargradient
 ]
```
```ocaml
type lineargradient_content = [ 
  | descriptive_element
  | `Animate
  | `AnimateTransform
  | `Set
  | `Stop
 ]
```
```ocaml
type lineargradient_attr = [ 
  | core_attr
  | presentation_attr
  | xlink_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `X1
  | `Y1
  | `X2
  | `Y2
  | `GradientUnits
  | `GradientTransform
  | `SpreadMethod
  | `Xlink_href
 ]
```
```ocaml
type radialgradient = [ 
  | `Radialgradient
 ]
```
```ocaml
type radialgradient_content = [ 
  | descriptive_element
  | `Animate
  | `AnimateTransform
  | `Set
  | `Stop
 ]
```
```ocaml
type radialgradient_attr = [ 
  | core_attr
  | presentation_attr
  | xlink_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `Cx
  | `Cy
  | `R
  | `Fx
  | `Fy
  | `GradientUnits
  | `GradientTransform
  | `SpreadMethod
  | `Xlink_href
 ]
```
```ocaml
type stop = [ 
  | `Stop
 ]
```
```ocaml
type stop_content = [ 
  | `Animate
  | `AnimateColor
  | `Set
 ]
```
```ocaml
type stop_attr = [ 
  | core_attr
  | presentation_attr
  | `Class
  | `Style
  | `Offset
 ]
```
```ocaml
type pattern = [ 
  | `Pattern
 ]
```
```ocaml
type pattern_content = [ 
  | animation_element
  | descriptive_element
  | shape_element
  | structural_element
  | gradient_element
  | `A
  | `AltGlyphDef
  | `ClipPath
  | `Color_Profile
  | `Cursor
  | `Filter
  | `Font
  | `Font_Face
  | `ForeignObject
  | `Image
  | `Marker
  | `Mask
  | `Pattern
  | `Script
  | `Style
  | `Switch
  | `Text
  | `View
 ]
```
```ocaml
type pattern_attr = [ 
  | conditional_processing_attr
  | core_attr
  | presentation_attr
  | xlink_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `ViewBox
  | `PreserveAspectRatio
  | `X
  | `Y
  | `Width
  | `Height
  | `PatternUnits
  | `PatternContentUnits
  | `PatternTransform
  | `Xlink_href
 ]
```
```ocaml
type clippath = [ 
  | `ClipPath
 ]
```
```ocaml
type clippath_attr = [ 
  | conditional_processing_attr
  | core_attr
  | presentation_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `Transform
  | `ClipPathUnits
 ]
```
```ocaml
type clippath_content = [ 
  | descriptive_element
  | animation_element
  | shape_element
  | `Text
  | `Use
 ]
```
```ocaml
type mask = [ 
  | `Mask
 ]
```
```ocaml
type mask_content = [ 
  | animation_element
  | descriptive_element
  | shape_element
  | structural_element
  | gradient_element
  | `A
  | `AltGlyphDef
  | `ClipPath
  | `Color_Profile
  | `Cursor
  | `Filter
  | `Font
  | `Font_Face
  | `ForeignObject
  | `Image
  | `Marker
  | `Mask
  | `Pattern
  | `Script
  | `Style
  | `Switch
  | `Text
  | `View
 ]
```
```ocaml
type mask_attr = [ 
  | conditional_processing_attr
  | core_attr
  | presentation_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `X
  | `Y
  | `Width
  | `Height
  | `MaskUnits
  | `MaskContentUnits
 ]
```
```ocaml
type filter = [ 
  | `Filter
 ]
```
```ocaml
type filter_content = [ 
  | descriptive_element
  | filter_primitive_element
  | `Animate
  | `Set
 ]
```
```ocaml
type filter_attr = [ 
  | core_attr
  | presentation_attr
  | xlink_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `X
  | `Y
  | `Width
  | `Height
  | `FilterRes
  | `FilterUnits
  | `PrimitiveUnits
  | `Xlink_href
 ]
```
```ocaml
type fedistantlight = [ 
  | `FeDistantLight
 ]
```
```ocaml
type fedistantlight_content = [ 
  | `Animate
  | `Set
 ]
```
```ocaml
type fedistantlight_attr = [ 
  | core_attr
  | `Azimuth
  | `Elevation
 ]
```
```ocaml
type fepointlight = [ 
  | `FePointLight
 ]
```
```ocaml
type fepointlight_content = [ 
  | `Animate
  | `Set
 ]
```
```ocaml
type fepointlight_attr = [ 
  | core_attr
  | `X
  | `Y
  | `Z
 ]
```
```ocaml
type fespotlight = [ 
  | `FeSpotLight
 ]
```
```ocaml
type fespotlight_content = [ 
  | `Animate
  | `Set
 ]
```
```ocaml
type fespotlight_attr = [ 
  | core_attr
  | `X
  | `Y
  | `Z
  | `PointsAtX
  | `PointsAtY
  | `PointsAtZ
  | `SpecularExponent
  | `LimitingConeAngle
 ]
```
```ocaml
type feblend = [ 
  | `FeBlend
 ]
```
```ocaml
type feblend_content = [ 
  | `Animate
  | `Set
 ]
```
```ocaml
type feblend_attr = [ 
  | core_attr
  | presentation_attr
  | filter_primitive_attr
  | `Class
  | `Style
  | `In
  | `In2
  | `Mode
 ]
```
```ocaml
type fecolormatrix = [ 
  | `FeColorMatrix
 ]
```
```ocaml
type fecolormatrix_content = [ 
  | `Animate
  | `Set
 ]
```
```ocaml
type fecolormatrix_attr = [ 
  | core_attr
  | presentation_attr
  | filter_primitive_attr
  | `Class
  | `Style
  | `Typefecolor
  | `Values
  | `In
 ]
```
```ocaml
type fecomponenttransfer = [ 
  | `FeComponentTransfer
 ]
```
```ocaml
type fecomponenttransfer_content = [ 
  | `FeFuncA
  | `FeFuncB
  | `FeFuncG
  | `FeFuncR
 ]
```
```ocaml
type fecomponenttransfer_attr = [ 
  | core_attr
  | presentation_attr
  | filter_primitive_attr
  | `Class
  | `Style
  | `In
 ]
```
```ocaml
type fefunca = [ 
  | `FeFuncA
 ]
```
```ocaml
type fefunca_content = [ 
  | `Animate
  | `Set
 ]
```
```ocaml
type fefunca_attr = [ 
  | core_attr
  | transfer_attr
 ]
```
```ocaml
type fefuncg = [ 
  | `FeFuncG
 ]
```
```ocaml
type fefuncg_content = [ 
  | `Animate
  | `Set
 ]
```
```ocaml
type fefuncg_attr = [ 
  | core_attr
  | transfer_attr
 ]
```
```ocaml
type fefuncb = [ 
  | `FeFuncB
 ]
```
```ocaml
type fefuncb_content = [ 
  | `Animate
  | `Set
 ]
```
```ocaml
type fefuncb_attr = [ 
  | core_attr
  | transfer_attr
 ]
```
```ocaml
type fefuncr = [ 
  | `FeFuncR
 ]
```
```ocaml
type fefuncr_content = [ 
  | `Animate
  | `Set
 ]
```
```ocaml
type fefuncr_attr = [ 
  | core_attr
  | transfer_attr
 ]
```
```ocaml
type fecomposite = [ 
  | `FeComposite
 ]
```
```ocaml
type fecomposite_content = [ 
  | `Animate
  | `Set
 ]
```
```ocaml
type fecomposite_attr = [ 
  | core_attr
  | presentation_attr
  | filter_primitive_attr
  | `Class
  | `Style
  | `In
  | `In2
  | `OperatorComposite
  | `K1
  | `K2
  | `K3
  | `K4
 ]
```
```ocaml
type feconvolvematrix = [ 
  | `FeConvolveMatrix
 ]
```
```ocaml
type feconvolvematrix_content = [ 
  | `Animate
  | `Set
 ]
```
```ocaml
type feconvolvematrix_attr = [ 
  | core_attr
  | presentation_attr
  | filter_primitive_attr
  | `Class
  | `Style
  | `In
  | `Order
  | `KernelMatrix
  | `Divisor
  | `Bias
  | `TargetX
  | `TargetY
  | `EdgeMode
  | `KernelUnitLength
  | `PreserveAlpha
 ]
```
```ocaml
type fediffuselighting = [ 
  | `FeDiffuseLighting
 ]
```
```ocaml
type fediffuselighting_content = [ 
  | descriptive_element
  | light_source_element
 ]
```
```ocaml
type fediffuselighting_attr = [ 
  | core_attr
  | filter_primitive_attr
  | presentation_attr
  | `Class
  | `Style
  | `In
  | `SurfaceScale
  | `DiffuseConstant
  | `KernelUnitLength
 ]
```
```ocaml
type fedisplacementmap = [ 
  | `FeDisplacementMap
 ]
```
```ocaml
type fedisplacementmap_content = [ 
  | `Animate
  | `Set
 ]
```
```ocaml
type fedisplacementmap_attr = [ 
  | core_attr
  | filter_primitive_attr
  | presentation_attr
  | `Class
  | `Style
  | `In
  | `In2
  | `Scale
  | `XChannelSelector
  | `YChannelSelector
 ]
```
```ocaml
type feflood = [ 
  | `FeFlood
 ]
```
```ocaml
type feflood_content = [ 
  | `Animate
  | `AnimateColor
  | `Set
 ]
```
```ocaml
type feflood_attr = [ 
  | core_attr
  | presentation_attr
  | filter_primitive_attr
  | `Class
  | `Style
 ]
```
```ocaml
type fegaussianblur = [ 
  | `FeGaussianBlur
 ]
```
```ocaml
type fegaussianblur_content = [ 
  | `Animate
  | `AnimateColor
  | `Set
 ]
```
```ocaml
type fegaussianblur_attr = [ 
  | core_attr
  | presentation_attr
  | filter_primitive_attr
  | `Class
  | `Style
  | `In
  | `StdDeviation
 ]
```
```ocaml
type feimage = [ 
  | `FeImage
 ]
```
```ocaml
type feimage_content = [ 
  | `Animate
  | `AnimateColor
  | `Set
 ]
```
```ocaml
type feimage_attr = [ 
  | core_attr
  | presentation_attr
  | filter_primitive_attr
  | xlink_attr
  | `Xlink_href
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `PreserveAspectRatio
 ]
```
```ocaml
type femerge = [ 
  | `FeMerge
 ]
```
```ocaml
type femerge_content = [ 
  | `FeMergeNode
 ]
```
```ocaml
type femerge_attr = [ 
  | core_attr
  | presentation_attr
  | filter_primitive_attr
  | `Class
  | `Style
 ]
```
```ocaml
type femorphology = [ 
  | `FeMorphology
 ]
```
```ocaml
type femorphology_content = [ 
  | `Animate
  | `Set
 ]
```
```ocaml
type femorphology_attr = [ 
  | core_attr
  | presentation_attr
  | filter_primitive_attr
  | `OperatorMorphology
  | `Class
  | `Style
  | `In
  | `Radius
 ]
```
```ocaml
type feoffset = [ 
  | `FeOffset
 ]
```
```ocaml
type feoffset_content = [ 
  | `Animate
  | `Set
 ]
```
```ocaml
type feoffset_attr = [ 
  | core_attr
  | presentation_attr
  | filter_primitive_attr
  | `Class
  | `Style
  | `Dx
  | `Dy
  | `In
 ]
```
```ocaml
type fespecularlighting = [ 
  | `FeSpecularLighting
 ]
```
```ocaml
type fespecularlighting_content = [ 
  | descriptive_element
  | light_source_element
 ]
```
```ocaml
type fespecularlighting_attr = [ 
  | core_attr
  | presentation_attr
  | filter_primitive_attr
  | `Class
  | `Style
  | `In
  | `SurfaceScale
  | `SpecularConstant
  | `SpecularExponent
  | `KernelUnitLength
 ]
```
```ocaml
type fetile = [ 
  | `FeTile
 ]
```
```ocaml
type fetile_content = [ 
  | `Animate
  | `Set
 ]
```
```ocaml
type fetile_attr = [ 
  | core_attr
  | presentation_attr
  | filter_primitive_attr
  | `Class
  | `Style
  | `In
 ]
```
```ocaml
type feturbulence = [ 
  | `FeTurbulence
 ]
```
```ocaml
type feturbulence_content = [ 
  | `Animate
  | `Set
 ]
```
```ocaml
type feturbulence_attr = [ 
  | core_attr
  | presentation_attr
  | filter_primitive_attr
  | `Class
  | `Style
  | `BaseFrequency
  | `NumOctaves
  | `Seed
  | `StitchTiles
  | `TypeStitch
 ]
```
```ocaml
type cursor = [ 
  | `Cursor
 ]
```
```ocaml
type cursor_content = descriptive_element
```
```ocaml
type cursor_attr = [ 
  | core_attr
  | conditional_processing_attr
  | xlink_attr
  | `X
  | `Y
  | `ExternalResourcesRequired
  | `Xlink_href
 ]
```
```ocaml
type a = [ 
  | `A
 ]
```
```ocaml
type a_content = [ 
  | animation_element
  | descriptive_element
  | shape_element
  | structural_element
  | gradient_element
  | `A
  | `AltGlyphDef
  | `ClipPath
  | `Color_Profile
  | `Cursor
  | `Filter
  | `Font
  | `Font_Face
  | `ForeignObject
  | `Image
  | `Marker
  | `Mask
  | `Pattern
  | `Script
  | `Style
  | `Switch
  | `Text
  | `View
 ]
```
```ocaml
type a_attr = [ 
  | core_attr
  | conditional_processing_attr
  | xlink_attr
  | graphical_event_attr
  | presentation_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `Transform
  | `Xlink_href
  | `Xlink_show
  | `Xlink_actuate
  | `Target
 ]
```
```ocaml
type view = [ 
  | `View
 ]
```
```ocaml
type view_content = descriptive_element
```
```ocaml
type view_attr = [ 
  | core_attr
  | `ExternalResourcesRequired
  | `ViewBox
  | `PreserveAspectRatio
  | `ZoomAndPan
  | `ViewTarget
 ]
```
```ocaml
type script = [ 
  | `Script
 ]
```
```ocaml
type script_content = [ 
  | `PCDATA
 ]
```
```ocaml
type script_attr = [ 
  | core_attr
  | xlink_attr
  | `ExternalResourcesRequired
  | `Type
  | `Xlink_href
 ]
```
```ocaml
type animate = [ 
  | `Animate
 ]
```
```ocaml
type animate_content = descriptive_element
```
```ocaml
type animate_attr = [ 
  | conditional_processing_attr
  | core_attr
  | animation_event_attr
  | xlink_attr
  | animation_attr_target_attr
  | animation_timing_attr
  | animation_value_attr
  | animation_addition_attr
  | `ExternalResourcesRequired
 ]
```
```ocaml
type set = [ 
  | `Set
 ]
```
```ocaml
type set_content = descriptive_element
```
```ocaml
type set_attr = [ 
  | core_attr
  | conditional_processing_attr
  | xlink_attr
  | animation_event_attr
  | animation_attr_target_attr
  | animation_timing_attr
  | `To
  | `ExternalResourcesRequired
 ]
```
```ocaml
type animatemotion = [ 
  | `AnimateMotion
 ]
```
```ocaml
type animatemotion_content = [ 
  | descriptive_element
  | `Mpath
 ]
```
```ocaml
type animatemotion_attr = [ 
  | conditional_processing_attr
  | core_attr
  | animation_event_attr
  | xlink_attr
  | animation_timing_attr
  | animation_value_attr
  | animation_addition_attr
  | `ExternalResourcesRequired
  | `Path
  | `KeyPoints
  | `Rotate
  | `Origin
 ]
```
```ocaml
type mpath = [ 
  | `Mpath
 ]
```
```ocaml
type mpath_content = descriptive_element
```
```ocaml
type mpath_attr = [ 
  | core_attr
  | xlink_attr
  | `ExternalResourcesRequired
  | `Xlink_href
 ]
```
```ocaml
type animatecolor = [ 
  | `AnimateColor
 ]
```
```ocaml
type animatecolor_content = descriptive_element
```
```ocaml
type animatecolor_attr = [ 
  | conditional_processing_attr
  | core_attr
  | animation_event_attr
  | xlink_attr
  | animation_attr_target_attr
  | animation_timing_attr
  | animation_value_attr
  | animation_addition_attr
  | `ExternalResourcesRequired
 ]
```
```ocaml
type animatetransform = [ 
  | `AnimateTransform
 ]
```
```ocaml
type animatetransform_content = descriptive_element
```
```ocaml
type animatetransform_attr = [ 
  | conditional_processing_attr
  | core_attr
  | animation_event_attr
  | xlink_attr
  | animation_attr_target_attr
  | animation_timing_attr
  | animation_value_attr
  | animation_addition_attr
  | `ExternalResourcesRequired
  | `Typeanimatetransform
 ]
```
```ocaml
type font = [ 
  | `Font
 ]
```
```ocaml
type font_attr = [ 
  | core_attr
  | presentation_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `HorizOriginX
  | `HorizOriginY
  | `HorizAdvX
  | `VertOriginX
  | `VertOriginY
  | `VertAdvY
 ]
```
```ocaml
type font_content = [ 
  | descriptive_element
  | `Font_Face
  | `Glyph
  | `Hkern
  | `MissingGlyph
  | `Vkern
 ]
```
```ocaml
type glyph = [ 
  | `Glyph
 ]
```
```ocaml
type glyph_content = [ 
  | animation_element
  | descriptive_element
  | shape_element
  | structural_element
  | gradient_element
  | `A
  | `AltGlyphDef
  | `ClipPath
  | `Color_Profile
  | `Cursor
  | `Filter
  | `Font
  | `Font_Face
  | `ForeignObject
  | `Image
  | `Marker
  | `Mask
  | `Pattern
  | `Script
  | `Style
  | `Switch
  | `Text
  | `View
 ]
```
```ocaml
type glyph_attr = [ 
  | core_attr
  | presentation_attr
  | `Class
  | `Style
  | `D
  | `HorizAdvX
  | `VertOriginX
  | `VertOriginY
  | `VertAdvY
  | `Unicode
  | `GlyphName
  | `Orientation
  | `ArabicForm
  | `Lang
 ]
```
```ocaml
type missingglyph = [ 
  | `MissingGlyph
 ]
```
```ocaml
type missingglyph_content = [ 
  | animation_element
  | descriptive_element
  | shape_element
  | structural_element
  | gradient_element
  | `A
  | `AltGlyphDef
  | `ClipPath
  | `Color_Profile
  | `Cursor
  | `Filter
  | `Font
  | `Font_Face
  | `ForeignObject
  | `Image
  | `Marker
  | `Mask
  | `Pattern
  | `Script
  | `Style
  | `Switch
  | `Text
  | `View
 ]
```
```ocaml
type missingglyph_attr = [ 
  | core_attr
  | presentation_attr
  | `Class
  | `Style
  | `D
  | `HorizAdvX
  | `VertOriginX
  | `VertOriginY
  | `VertAdvY
 ]
```
```ocaml
type hkern = [ 
  | `Hkern
 ]
```
```ocaml
type hkern_attr = [ 
  | core_attr
  | `U1
  | `G1
  | `U2
  | `G2
  | `K
 ]
```
```ocaml
type vkern = [ 
  | `Vkern
 ]
```
```ocaml
type vkern_attr = [ 
  | core_attr
  | `U1
  | `G1
  | `U2
  | `G2
  | `K
 ]
```
```ocaml
type font_face = [ 
  | `Font_Face
 ]
```
```ocaml
type font_face_content = [ 
  | descriptive_element
  | `Font_Face_Src
 ]
```
```ocaml
type font_face_attr = [ 
  | core_attr
  | `Font_Family
  | `Font_Style
  | `Font_Variant
  | `Font_Weight
  | `Font_Stretch
  | `Font_Size
  | `UnicodeRange
  | `UnitsPerEm
  | `Panose1
  | `Stemv
  | `Stemh
  | `Slope
  | `CapHeight
  | `XHeight
  | `AccentHeight
  | `Ascent
  | `Descent
  | `Widths
  | `Bbox
  | `Ideographic
  | `Alphabetic
  | `Mathematical
  | `Hanging
  | `VIdeographic
  | `VAlphabetic
  | `VMathematical
  | `VHanging
  | `UnderlinePosition
  | `UnderlineThickness
  | `StrikethroughPosition
  | `StrikethroughThickness
  | `OverlinePosition
  | `OverlineThickness
 ]
```
```ocaml
type font_face_src = [ 
  | `Font_Face_Src
 ]
```
```ocaml
type font_face_src_content = [ 
  | `Font_Face_Name
  | `Font_Face_Uri
 ]
```
```ocaml
type font_face_src_attr = core_attr
```
```ocaml
type font_face_uri = [ 
  | `Font_Face_Uri
 ]
```
```ocaml
type font_face_uri_content = [ 
  | `Font_Face_Format
 ]
```
```ocaml
type font_face_uri_attr = [ 
  | core_attr
  | xlink_attr
  | `Xlink_href
 ]
```
```ocaml
type font_face_format = [ 
  | `Font_Face_Format
 ]
```
```ocaml
type font_face_format_attr = [ 
  | core_attr
  | `String
 ]
```
```ocaml
type font_face_name = [ 
  | `Font_Face_Name
 ]
```
```ocaml
type font_face_name_attr = [ 
  | core_attr
  | `Name
 ]
```
```ocaml
type metadata = [ 
  | `Metadata
 ]
```
```ocaml
type metadata_attr = [ 
  | core_attr
 ]
```
```ocaml
type foreignobject = [ 
  | `ForeignObject
 ]
```
```ocaml
type foreignobject_attr = [ 
  | core_attr
  | conditional_processing_attr
  | graphical_event_attr
  | presentation_attr
  | `Class
  | `Style
  | `ExternalResourcesRequired
  | `Transform
  | `X
  | `Y
  | `Width
  | `Height
 ]
```
```ocaml
type alignment_baseline = [ 
  | `After_edge
  | `Alphabetic
  | `Auto
  | `Baseline
  | `Before_edge
  | `Central
  | `Hanging
  | `Ideographic
  | `Inherit
  | `Mathematical
  | `Middle
  | `Text_after_edge
  | `Text_before_edge
 ]
```
```ocaml
type dominant_baseline = [ 
  | `Auto
  | `Use_script
  | `No_change
  | `Reset_size
  | `Ideographic
  | `Alphabetic
  | `Hanging
  | `Mathematical
  | `Central
  | `Middle
  | `Text_after_edge
  | `Text_before_edge
  | `Inherit
 ]
```
```ocaml
type in_value = [ 
  | `SourceGraphic
  | `SourceAlpha
  | `BackgroundImage
  | `BackgroundAlpha
  | `FillPaint
  | `StrokePaint
  | `Ref of string
 ]
```
```ocaml
type offset = [ 
  | `Number of number
  | `Percentage of percentage
 ]
```
```ocaml
type big_variant = [ 
  | `A
  | `Absolute_colorimetric
  | `Align
  | `Always
  | `Atop
  | `Arithmetic
  | `Auto
  | `B
  | `Bevel
  | `Blink
  | `Butt
  | `CSS
  | `Darken
  | `Default
  | `Dilate
  | `Disable
  | `Discrete
  | `Duplicate
  | `End
  | `Erode
  | `Exact
  | `FractalNoise
  | `Freeze
  | `HueRotate
  | `G
  | `Gamma
  | `GeometricPrecision
  | `H
  | `Identity
  | `In
  | `Inherit
  | `Initial
  | `Isolated
  | `Lighten
  | `Line_through
  | `Linear
  | `LuminanceToAlpha
  | `Magnify
  | `Matrix
  | `Medial
  | `Middle
  | `Miter
  | `Multiply
  | `Never
  | `New
  | `None
  | `Normal
  | `NoStitch
  | `ObjectBoundingBox
  | `OnLoad
  | `OnRequest
  | `OptimizeLegibility
  | `OptimizeSpeed
  | `Other
  | `Out
  | `Over
  | `Overline
  | `Paced
  | `Pad
  | `Perceptual
  | `Preserve
  | `R
  | `Reflect
  | `Remove
  | `Repeat
  | `Replace
  | `Relative_colorimetric
  | `Rotate
  | `Round
  | `Saturate
  | `Saturation
  | `Scale
  | `Screen
  | `SkewX
  | `SkewY
  | `Spacing
  | `SpacingAndGlyphs
  | `Spline
  | `Square
  | `Start
  | `Stitch
  | `Stretch
  | `StrokeWidth
  | `Sum
  | `Table
  | `Terminal
  | `Translate
  | `Turbulence
  | `Underline
  | `UserSpaceOnUse
  | `V
  | `WhenNotActive
  | `Wrap
  | `XML
  | `Xor
 ]
```

### Deprecated

```ocaml
type animation = animate
```
deprecated Use animate instead
```ocaml
type animation_content = animate_content
```
deprecated Use animate\_content instead
```ocaml
type animation_attr = animate_attr
```
deprecated Use animate\_attr instead