# 5.0.0

TyXML now follows the current specifications: the WHATWG living standard for
HTML, and SVG 2 with the Filter Effects module for SVG, whose support had not
moved since SVG 1.1.

## Breaking changes

* The `hidden` attribute takes an enumerated argument
  (`` [`Hidden | `Until_found] ``) instead of no argument, and
  `contenteditable` takes an enumerated argument
  (`` [`True | `False | `Plaintext_only] ``) instead of a boolean, so that the
  until-found and plaintext-only states can be expressed
* `a_itemtype` and `a_ping` take an `Xml.uri list` instead of a `string list`
  and go through `Xml.uris_attrib`, like every other URL-valued attribute.
  `Tyxml.Html` is unaffected, its `Xml.uri` is `string`
* The SVG attributes whose value is a URL follow the same rule: `a_href`,
  `a_xlink_href`, `a_xml_base`, `a_xlink_role`, `a_xlink_arcrole` and the
  `a_ping` this release adds to the SVG `a` element take an `Xml.uri`, where
  they used to take the `iri` type, which is `string`. The presentation
  attributes that accept a funciri such as `url(#id)` or a keyword such as
  `none`, that is `clip-path`, `mask` and the `marker-*` ones, keep `iri`
* SVG documents are printed without a doctype. `Svg.Info` declared SVG 1.1 and
  every printed document carried the SVG 1.1 DTD, which forbids the SVG 2
  elements and attributes TyXML now emits, and SVG 2 defines no DTD.
  `Info.version` and `Info.standard` name SVG 2
* Unlike their HTML counterparts, the `Svg_types.*_content` types are widened
  along with the content models they describe: the SVG element functions use
  them directly, there is no `_content_fun` on that side. Code that names one
  of them and constrains it with `=` has to be updated
* Misspelled names are corrected: `a_baseFrenquency` becomes `a_baseFrequency`,
  `a_externalRessourcesRequired` becomes `a_externalResourcesRequired`, the
  `stroke-linejoin` value `` `Bever `` becomes `` `Bevel ``, and the types
  `Html_types.subressource_integrity` and `Svg_types.paint_whitout_icc` become
  `subresource_integrity` and `paint_without_icc`
* The newly deprecated elements and attributes are an error, and not a warning,
  for the users of the PPX and the JSX syntax who build in dune's dev profile,
  where alerts are fatal. The alert is reported on the markup literal, so it
  has to be silenced on the enclosing definition
* The `Wrapped_functions` module type has four new functions, which the
  implementations of the functorial interface must provide:
  `string_of_blocking` and `string_of_ol_type` for HTML, `unoption_string`
  and `string_of_semicolonstrings` for SVG
* The build now requires OCaml 4.08, dune 3.18 and ppxlib 0.36
  (Sora Morimoto, and #340 by Patrick Ferris for the OCaml 5.2 AST)
* The test matrix covers OCaml 4.14 and the 5.x series. The lower bound stays
  at 4.08, but the versions from 4.08 to 4.13 are no longer tested

## HTML

* New elements: `s`, `bdi`, `search`, `data`, `slot`, and `track` with its
  `kind`, `srclang` (as `a_track_srclang`) and `default` attributes, `audio`
  and `video` taking an optional `?tracks` argument. `meta_itemprop` is the
  form of `meta` carrying an `itemprop` attribute, which the specification
  allows wherever phrasing content is expected and not only in the head
  (#343 by toastal)
* New global attributes: `popover`, `inert`, `enterkeyhint`, `autocapitalize`,
  `autocorrect`, `writingsuggestions`, `nonce`, `slot` and `is`, the microdata
  attributes `itemscope`, `itemtype`, `itemid`, `itemprop` and `itemref`, the
  CSS shadow parts attributes `part` and `exportparts`, and the `` `Auto ``
  value of `dir`
  (#341 by @SylvainBoilard for popover, #343 by toastal for microdata)
* New attributes on elements: `popovertarget` and `popovertargetaction` on
  buttons and inputs, `command` and `commandfor` (invoker commands) on
  buttons, `loading`, `decoding` and `fetchpriority` on images, `srcdoc`,
  `allow` and `loading` on iframes, `as`, `imagesrcset` and `imagesizes` on
  links (`a_as`), `nomodule` on scripts, `blocking` on links, scripts and styles,
  `ping` on `a` and `area`, `dirname` on inputs and textareas, `capture`
  (W3C HTML Media Capture) on inputs, `a_ol_type` on ordered lists, `abbr` on
  th cells, `closedby` on dialogs, `media` on meta, `name` on `details`,
  `playsinline` and `disablepictureinpicture` on videos,
  `disableremoteplayback` on audios and videos, and the declarative shadow DOM
  attributes on templates (`shadowrootmode`, `shadowrootdelegatesfocus`,
  `shadowrootclonable` and `shadowrootserializable`). Also allow `crossorigin`,
  `usemap` and `referrerpolicy` on images, `referrerpolicy` on `a` and `area`,
  and `autocomplete` on selects and textareas
  (#341 by @SylvainBoilard for `name` on `details`, fixes #268)
* New event handler attributes: the pointer events (`a_onpointerdown` and
  its siblings), `a_onwheel`,
  `a_onauxclick`, and the global handlers that were missing
  (`a_onbeforeinput`, `a_onbeforematch`, `a_onbeforetoggle`, `a_oncancel`,
  `a_oncontextlost`, `a_oncontextrestored`, `a_oncopy`, `a_oncut`,
  `a_onpaste`, `a_oncuechange`, `a_onscrollend`,
  `a_onsecuritypolicyviolation`, `a_onslotchange` and `a_ontoggle`), plus
  `a_onlanguagechange`, `a_onrejectionhandled` and `a_onunhandledrejection`
  on body
* New attribute values: `dialog` for `method` and `formmethod`, and the newer
  `sandbox` tokens (`allow-downloads`, `allow-modals`,
  `allow-orientation-lock`, `allow-popups-to-escape-sandbox`,
  `allow-presentation`, `allow-top-navigation-by-user-activation` and
  `allow-top-navigation-to-custom-protocols`)
* Content models brought in line with the specification: `div` groups `dt` and
  `dd` inside `dl`, the script-supporting elements (`script`, `template`) are
  accepted in `dl`, `ol`, `ul`, `menu`, `table` and its row groups, `tr`,
  `select` and `optgroup`, `hgroup` accepts `p`, and `select` accepts `hr`.
  The `_content` types keep their previous value, only the types the element
  functions use are widened
  (#344 by toastal, fixes #342 and #321)
* Undeprecate the `scope` attribute, which is valid on table header cells in
  the living standard
* Deprecate `a_version`, `a_xml_space` and `a_scrolling`, which produce a type
  tag that is in no attribute category, so no element ever accepted them

## SVG

* New elements: `mask`, `feMergeNode` and `feDropShadow`. `mask` was declared
  in `Svg_types` but the element itself was missing, and `feMerge` could be
  given no child at all since `feMergeNode` did not exist
* Presentation attributes: add the ones whose type tags were already declared
  but had no function (`baseline-shift`, `clip-rule`, `color`,
  `color-interpolation`, `color-interpolation-filters`, `color-rendering`,
  `cursor`, `direction`, `display`, `fill-opacity`, `filter`, `flood-color`,
  `flood-opacity`, `font-size-adjust`, `image-rendering`, `letter-spacing`,
  `lighting-color`, `marker-end`, `marker-mid`, `marker-start`, `mask`,
  `opacity`, `overflow`, `pointer-events`, `shape-rendering`, `unicode-bidi`,
  `visibility`, `word-spacing` and `writing-mode`) and the ones new in SVG 2
  (`paint-order`, `text-overflow`, `transform-origin`, `vector-effect` and
  `white-space`). The ones SVG 2 removed are added as deprecated: `clip`,
  `color-profile`, `enable-background`, `glyph-orientation-horizontal`,
  `glyph-orientation-vertical` and `kerning`
  (#333 by Martin @MBodin Bodin for `clip-path`)
* Other new attributes: the `tabindex` and `autofocus` global attributes,
  `lang` becomes global, ARIA support (`a_role` and `a_aria`), the SVG 2 link
  attributes on the `a` element (`download`, `hreflang`, `ping`,
  `referrerpolicy`, `rel` and `type`), `crossorigin`, `decoding` and
  `fetchpriority`, `fr` on radial gradients, `side` and `path` on `textPath`,
  and the SVG 2 geometry attributes on `symbol` (`x`, `y`, `width`, `height`,
  `refX`, `refY`)
* Attribute functions missing for type tags that were already declared:
  `a_end` (SMIL timing), `a_z` (light sources), `a_filterUnits`, `a_title` (on
  `style`), `a_origin`, `a_panose_1`, `a_descent`, and the deprecated
  `a_xlink_type`, `a_xlink_role` and `a_xlink_arcrole`
* New values of existing attributes: `miter-clip` and `arcs` for
  `stroke-linejoin`, all the CSS blend modes for the `mode` attribute of
  `feBlend`, and `text-top` and `text-bottom` for `alignment-baseline` and
  `dominant-baseline`
* The HTML event handler attributes, which SVG 2 reuses on every element
  (focus, keyboard, pointer, wheel, clipboard, drag and media events), and the
  window event handler attributes on the `svg` element
* Content models widened to SVG 2: shapes accept paint servers, `clipPath`,
  `marker`, `mask`, `script` and `style`; `use` and `image` accept `clipPath`,
  `mask`, `script` and `style`; text elements accept paint servers, `script`
  and `style`; filter primitives accept descriptive elements, `animate`,
  `script` and `set`; gradients, `stop` and `clipPath` accept `script`
* Separate the values of the `values`, `keyTimes` and `keySplines` animation
  attributes with semicolons, as SMIL requires, instead of commas
  (#308 by @rand00)
* Deprecate what SVG 2 removed and was not deprecated yet:
  `externalResourcesRequired`, `filterRes`, `zoomAndPan`, the `onzoom`,
  `onactivate`, `onfocusin` and `onfocusout` event handlers, and the `cursor`
  and `animateColor` elements. Conversely, undeprecate `a_onload`: the load
  event is still fired on SVG elements

## Fixes

* The `area` element could not be used at all: it had no `href` attribute, and
  its type tag was in no content model, so a `map` containing areas fitted
  nowhere. It is now phrasing content, and its signature uses `area_attrib`
  instead of repeating a shorter list that left out `download`, `ping` and
  `referrerpolicy`
  (fixes part of #184)
* The `li` children of `menu` could not be built: the `` `Lis `` branch
  required an `li` whose attributes were a subset of the common ones,
  excluding the `value` attribute that `li` always allows
* Several attribute names were emitted misspelled: `reversed` (emitted
  `reserved`), the `allow-scripts` sandbox token (`allow-script`) and, in SVG,
  `zoomAndPan`, `requiredExtensions`, `externalResourcesRequired`, `edgeMode`
  (emitted `targetY`), `filterRes`, `target` (emitted `xlink:target`),
  `glyph-name` and the `font-face-format` element (emitted `font-face-uri`).
  Several SVG attribute values were too: the hyphens missing from the
  `rendering-intent` and `dominant-baseline` values, the case of the `in` and
  `in2` keywords (`SourceGraphic` and the other built-in inputs), and
  `` `Align ``, emitted as the empty string
  (Hugo @hhugo Heuzard)
* Several type tags did not match their attribute, making `a_edgeMode`,
  `a_preserveAlpha`, `a_filterRes`, `a_target`, `a_glyph_name`,
  `a_arabic_form`, `a_requiredExtensions`, `a_xlink_title` and `a_low`
  unusable, and some element type tags were misspelled or duplicated
  (`feFuncR`, `feFuncG` and `feFuncB` were all defined as `` `FeFuncA ``)
  (Hugo @hhugo Heuzard)
* The `symbol` element accepted neither the core attributes, so no `id`, which
  made it impossible to reference, nor the presentation and event attributes,
  nor shape children
* The `xml:base`, `xml:lang` and `xml:space` attributes and the touch event
  handler attributes of SVG had type tags listed in no attribute category,
  which made them unusable on every element
* `Xml_print.compose_decl` emitted a literal `\n` instead of a newline, which
  made the XML declaration invalid
  (#348 by Martin @MBodin Bodin)
* In the JSX syntax, `Html` used as an element name was mistaken for a
  user-defined component: the test lowercased the name and then compared it
  with a capitalised string, so it never matched
* Fix the development-profile build with recent compilers by anonymizing the
  unused functor parameters of the printer and functor signatures (warning 67)
* Fix the typo `whitout` in a type definition
  (#324 by Martin @MBodin Bodin)
* Fix the typo `subresource` and various English orthographic mistakes
  (#345 by toastal)
* `Unsafe` gains `uris_attrib`, the escape hatch that was missing for an
  attribute holding a space-separated list of URLs, next to `uri_attrib`

## PPX and JSX syntax

* No camel case SVG attribute was recognised, so `viewBox`, `maskUnits`,
  `stdDeviation`, `gradientTransform`, `preserveAspectRatio`, `refX`,
  `markerWidth` and many others were rejected
* Whitespace between SVG tags is ignored for the elements whose content model
  does not accept text. Indented SVG, as the specification itself writes it,
  was rejected: `[%svg "<g> </g>"]` failed to typecheck. Whitespace is still
  significant in `text`, `tspan`, `textPath`, `desc`, `title`, `style` and
  `script`
  (#331 by Martin @MBodin Bodin, fixes #330)
* A `transform` attribute whose arguments were comma separated, such as
  `translate(1,1)`, was rejected. Any whitespace, and not only the space
  character, is now accepted around the separators of list-valued attributes
  (#328 by Martin @MBodin Bodin, fixes #326)
* Two transforms must now be separated by whitespace or a comma, as the
  grammar of the attribute requires. `transform="translate(1,2)rotate(45)"`
  used to be accepted and silently turned into valid output
* The `values`, `keyTimes` and `keySplines` animation attributes are split on
  semicolons, where they used to be split on spaces
  (#308 by @rand00)
* An empty `option` is accepted, as for `script` and `textarea`: an option
  carrying a `label` and a `value` attribute has no content, which is the
  usual form inside a `datalist`
  (fixes #228)

## Documentation

* The manual is converted from wikicreole to odoc, and the API references in
  the interfaces are native odoc references. The themed site published on
  ocsigen.org is built by wodoc
  (#352 by Hugo @hhugo Heuzard for the rendering)

# 4.6.0

* Update for OCaml 5.0 and drop support for OCaml 4.2.0
  (#312 by @rr0gi)

* Add additional variants to `linktype` for the `rel` attribute
  (Leon @LogicalOverflow Vack)

* Expand options for `autocomplete` attribute on `<input>` elements
  (#302 by Aron @aronerben Erben)

* Fix the SVG element `<animate>` (by the way, deprecate `animation` et
  al. in favor of `animate` et al.)
  (#306 by Idir @ilankri Lankri)

* Add support for `dialog` element and `onclose` attribute
  (#301 by Julien Sagot)
* Add an escape hatch for emitting attributes with non-standard names
  in jsx or ppx code (a leading `_` character on attribute name)
  (#295 Chas @cemerick Emerick)
* Add support for `type` attribute on `<script>` elements
  (#293 by Ulrik @ulrikstrid Strid and Chas @cemerick Emerick)

* Add svg `fill-rule` attribute
  (#294 by Eric @dedbox Griffis)

# 4.5.0

* Move all the PPXs to ppxlib
  (#271, Initial code by Sonja @pitag-ha Heinze)

* Add the `translate` attribute
  (#281 by Javier @jchavarri Chávarri)
* Update allowed `inputmode`s
  (#279 by Joel @joelburget Burget)
* Add the `picture` element
  (#263 by Stéphane @slegrand45 Legrand)

# 4.4.0

* Add support for Reason's JSX syntax with a new `tyxml-jsx` package
  (#254 by Joris Giovannangeli and Gabriel Radanne
   with help from Ulrik Strid and Louis Roché)
* Modernize the handling of toplevel printers for utop.
  (Gabriel Radanne)

## Elements and attributes

* Add `allowfullscreen`, `allowpaymentrequest`, `referrerpolicy` attributes
  (#242 by Thibault Suzanne)
* Allow `crossorigin` attribute for script element
  (#243 by Thibault Suzanne)
* Greatly improved support of whitespaces in the PPX
  (#225 by Jules Aguillon)
* Add preliminary support for ARIA attributes
  (#253 by Stéphane Legrand and Gabriel Radanne)
* Add `template` element
  (#239 Stéphane Legrand)

* Several bug fixes for types and PPX

# 4.3.0

* Dunify
  This also removes all the deprecated libraries (`tyxml.syntax`, `tyxml.parser`)
  and removes the ocamlfind library `tyxml.ppx` in favor of `tyxml-ppx`.
  (#197 by Drup, Rudi Grinberg and Anton Bachin)
* Add simplistic indentation for the Format-based printer (#187 by Drup)
* Allow the ppx to be used for more exotic tyxml instances, such
  as reactive elements (#200 by Drup)
* Add `Html.of_seq` and `Svg.of_seq`, which allow to easily import
  HTML parsed with markup in TyXML (#221 by Drup)

## Elements and attributes
* Add Html.txt and Svg.txt as an alias for `pcdata` (#222 by Drup)
* Add noopener link types (#198 by Jérôme Vouillon)
* Slightly relax dt content type (#193 by Anton Bachin)
* Add touch events (#211 by Malthe Borch)
* Fix handling of figcaption in the PPX (#219 by Drup)

# 4.2.0

* Compatibility with OCaml 4.6.0.
* The ppx should now be compatible with driver-based workflows. In particular, jbuilder.
* Future breakage:
* The two camlp4-based packages (tyxml.syntax and tyxml.parser) are now deprecated and will be removed in the next major version.
* Introduction of the tyxml-ppx ocamlfind package. Usage of the tyxml.ppx package is discouraged, and it will be removed in the next major version.
* Various fixes in the Html_sigs.T module (contribution by Fabian Pijcke):
* Fixed the map element function signature.
* The elements functions now (almost) all make use of the types defined in Html_types, rather than redefining them.
* Html_sigs.T.fieldset now takes [< legend] elt wrap as optional argument rather than legend elt wrap.
* Add basic support for `aria-*` attributes (contribution by Armaël Guéneau)
(see https://www.w3.org/TR/wai-aria-1.1/#states_and_properties)
* Add support for the `role` attribute (contribution by Armaël Guéneau)
(see https://www.w3.org/TR/role-attribute/)
* Add support for the `minlength` form attribute (contribution by Armaël Guéneau)
(See https://www.w3.org/TR/html5/forms.html#attr-input-minlength)

# 4.1.0

* Uses uutf 1.0 (contribution by Daniel Bunzli)

# 4.0.1

* Fix handling of comments in the ppx.
* Fix printing of utf8 in attributes.
* Properly flush ppx errors. This bug was causing some blank error messages.
* Fix handling of whitespaces in `<select>` in the ppx.

# 4.0.0

## Features
* A new PPX has been added that allows to use tyxml with the HTML/SVG syntax, using the markup library (contribution by Anton 'aantron' Bachin).
* New Format-based printers are available, both as functors and has `pp` functions in the various implementations. Old printers are deprecated.
* Toplevel printers are now available for the `tyxml` library.
* The `str` library has been replaced by `re`.
* Various attributes arguments have been simplified. In particular:
* Constants arguments have been removed
* `` `On|`Off `` arguments are now replaced by booleans.
* Some arguments now use an option type.

* XML comments are now properly serialized (without escaping but with sanitization).

## Elements and attributes
* Add srcset and sizes attributes (contribution by Stéphane 'slegrand45' Legrand).
* The arguments of the `sandbox` attributes are now more consistent (contributino by Anton 'aantron' Bachin).
* Various SVG attributes and elements are now properly named (contributino by Anton 'aantron' Bachin).
* Add inputmode attribute.

## Documentation
* Both the API documentation and the manual have been completely rewritten! Do not hesitate to read them and provide feedback.
* Various examples have been added in the `example/` directory. (basic_website contributed by Edgar 'fxfactorial' Aroutiounian).

## Renaming and deprecations
* Files in the `tyxml` library are now packed in a `Tyxml` module.
If you were using one of `Html5`, `Svg` and `Xml` module, simply open `Tyxml`.
* All Html5 modules are now named Html
* Various attributes and elements have been renamed. The original versions have been kept and marked deprecated.
* Various elements that were both deprecated in the HTML specification and not usable due to typing constraints have been removed.


# 3.6.0

* Improves and simplify the wrapping interface. Breaking change.
* Add the possibility to specify converters, for constants functions.
See also eliom's shared react.
* Fix printing of floating numbers.
* Add the main element.
* Fix the accept attribute.

# 3.5.0

* Add Tyxml_name, which allows to derive tyxml identifiers from HTML
elements and attributes.
* Internally build the tool `autoname`, which applies the aftermentionned
transformation for the given elements/attributes.
* Fix typo in `datetime-local`.
* Add download attributes for area and tags.
* Add various svg `text` attributes.
* Fix namespaces issues related to svg elements inside html.

# 3.4.0

* Add `a_lang` for HTML. Deprecate `a_srclang` in favor of `a_xml_lang`.
* Fix a performance issue with `Xml_print.Utf8.{normalize, normalize_html}.
* Remove `Xml_print.Utf8.normalize_from`.
The function was not useful and not optimizable easily.
* Add missing parameters for the attributes xlink:actuate and xml:space.
* Svg elements use the xlink namespace (contribution by Florent Becker).
* Do not use the `url(...)` form when inappropriate (contribution by Florent Becker).
* Fix a typo in the `spellcheck` attribute (contribution by Kevin Brubeck Unhammer).
* Fix the `sizes` attributes and add missing attributes for the `sandbox` tag (contributions by Eyyüb Sari).
* Fix the `img` tag in the syntax extension.
* Fix compilation of the opam package under freeBSD.
* Fix typing for the various `font_` svg attributes.

# 3.3.0

* Add `Xml_print.Utf8` to encode html elements to utf8 properly.

# 3.2.1

* Add signature functors to ease export of module created with the functorial interface.
See the manual for more details.
* Fix variance for Svg.attrib.
* Fix export of Xml.list_wrap for Html5.M and Svg.M. Should fix syntax extension with those.

# 3.2.0

* Remove Xhtml.
* Remove plus elements.
For example, table doesn't enforce non-emptyness anymore.
* Add various types that were not exported (img, dl, figure, rp, rt and ruby types).
* Expose `string_of_number`, the better stringifier introduced in 3.1.0.

* Functorial interface breaking changes:
* Modify the functorized interface to export typed events.
You can now specify different handlers for keyboard and mouse events.
There are two new types and functions (keyboard and mous events) in the Xml signature.
* Add a wrapping type for lists of nodes. See reactiveData and new eliom wrapping.

# 3.1.0

* Replace fake booleans (`\`True | \`False`) by actual booleans. Breaking change.
* Camlp4 is now an optional dependency. The syntax extension is build only when the syntax flag is enabled (true by default).
* Use a better stringifier for float values (copied from js_of_ocaml).
* Add attributes `muted`, `crossorigin` and `mediagroup` for `<audio>` and `<video>`.
* Various misc fixes
* Fix in the svg syntax extension
* Typo "proress" -> "progress"

# 3.0.0

* In the functorial interface, allow to wrap xml nodes inside a monad by providing an additional wrapping module. Used by reactive nodes in eliom.
* Use oasis as build systems
* Various bug fixes, mostly related to the svg module.

# 2.3.0

* Adding module Unsafe for inserting missing nodes or attributes

# 2.2.0

* Adding tag <u>

# 2.1.0

* Rename all module names lower-case
* Explicit choice of implementation for syntax extension

# 2.0.2

* Add a simple printer: XML.print
* API change:
** Rename XML.event into XML.event_handler
** The functorized interface now export the concrete representation of XML.nodes
** Open types in SVG_sigs (closes #269).

# 2.0.1

* Allow compilation on win32/msvc
* Truly allow to abstract the XMl.uri representation
* Always print XHTML in a format that is "Html compatible":
** Add missing namespace in <html>.

# 2.0

* Allow the namespace attribute in HTML5 elements.
* Always print polyglot XML. ( document correct as HTML5 and as XML )
* Add new polymorphic types to HTML5 ( between_phrasing_and_phrasing_without_interactive, ... )

# 2.0-rc1

* Some fixes to match the latest HTML5 working draft (05/08/2011).
* Simplification of the functorial interface (use module substitution)
* Fix IFrame usage.

# 1.91

* First independent release (was released with ocsigen)
* Add a functorial interface for concrete XML representation
* Add a functorial interface for stream printer
* Rename XHTML5 into HTML5
* Change the default syntax to HTML5 instead of XHTML 1.1
