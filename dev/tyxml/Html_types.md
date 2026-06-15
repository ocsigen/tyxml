
# Module `Html_types`

HTML types with variants, goes with [`Html_sigs.T`](./Html_sigs-module-type-T.md).

see [http://dev.w3.org/html5/spec/Overview.html](http://dev.w3.org/html5/spec/Overview.html) information concerning HTML at W3C.

## Attribute types.

```ocaml
type cdata = string
```
Character data

```ocaml
type id = string
```
A document-unique identifier

```ocaml
type idref = string
```
A reference to a document-unique identifier

```ocaml
type idrefs = idref list
```
A space-separated list of references to document-unique identifiers

```ocaml
type name = string
```
A name with the same character constraints as ID above

```ocaml
type nmtoken = string
```
A name composed of only name tokens as defined in XML 1\.0

see [http://www.w3.org/TR/2000/REC-xml-20001006](http://www.w3.org/TR/2000/REC-xml-20001006) XML 1.0
```ocaml
type nmtokens = nmtoken list
```
One or more whitespace-separated NMTOKEN values


### Data Types

```ocaml
type character = char
```
A single character from ISO 10646\.

```ocaml
type charset = string
```
A character encoding, as per RFC2045 (MIME).

see [http://tools.ietf.org/html/rfc2045](http://tools.ietf.org/html/rfc2045) RFC2045
```ocaml
type charsets = charset list
```
A space-separated list of character encodings, as per RFC2045 (MIME).

see [http://tools.ietf.org/html/rfc2045](http://tools.ietf.org/html/rfc2045) RFC2045
```ocaml
type contenttype = string
```
A media type, as per RFC2045 (MIME).

see [http://tools.ietf.org/html/rfc2045](http://tools.ietf.org/html/rfc2045) RFC2045
```ocaml
type contenttypes = contenttype list
```
A comma-separated list of media types, as per RFC2045 (MIME).

see [http://tools.ietf.org/html/rfc2045](http://tools.ietf.org/html/rfc2045) RFC2045
```ocaml
type number = int
```
```ocaml
type numbers = number list
```
```ocaml
type coords = string list
```
Comma- separated list of coordinates to use in defining areas.

```ocaml
type datetime = string
```
Date and time information.

```ocaml
type number_or_datetime = [ 
  | `Number of number
  | `Datetime of datetime
 ]
```
Either a number or date and time information.

```ocaml
type fpi = string
```
A character string representing an SGML Formal Public Identifier.

```ocaml
type frametarget = string
```
Frame name used as destination for results of certain actions.

```ocaml
type languagecode = string
```
A language code, as per RFC5646/BCP47.

see [http://tools.ietf.org/html/rfc5646](http://tools.ietf.org/html/rfc5646) RFC5646
```ocaml
type linktype = [ 
  | `Alternate
  | `Archives
  | `Author
  | `Bookmark
  | `Canonical
  | `Dns_prefetch
  | `External
  | `First
  | `Help
  | `Icon
  | `Index
  | `Last
  | `License
  | `Manifest
  | `Me
  | `Modulepreload
  | `Next
  | `Nofollow
  | `Noopener
  | `Noreferrer
  | `Opener
  | `Pingback
  | `Preconnect
  | `Prefetch
  | `Prev
  | `Preload
  | `Prerender
  | `Search
  | `Stylesheet
  | `Sidebar
  | `Tag
  | `Up
  | `Other of string
 ]
```
```ocaml
type linktypes = linktype list
```
Authors may use the following recognized link types, listed here with their conventional interpretations. A LinkTypes value refers to a space-separated list of link types. White space characters are not permitted within link types. These link types are case-insensitive, i.e., `"Alternate"` has the same meaning as `"alternate"`.

User agents, search engines, etc. may interpret these link types in a variety of ways. For example, user agents may provide access to linked documents through a navigation bar.

- ``Alternate`: Gives alternate representations of the current document.
- ``Archives`: Provides a link to a collection of records, documents, or other materials of historical interest.
- ``Author`: Gives a link to the current document's author.
- ``Bookmark`: Gives the permalink for the nearest ancestor section.
- ``Canonical`: Gives the preferred location for accessing the current document.
- ``Dns_prefetch`: Specifies that the user agent should preemptively perform DNS resolution for the target resource's origin..
- ``External`: Indicates that the referenced document is not part of the same site as the current document.
- ``First`: Indicates that the current document is a part of a series, and that the first document in the series is the referenced document.
- ``Help`: Provides a link to context-sensitive help.
- ``Icon`: Imports an icon to represent the current document.
- ``Index`: Gives a link to the document that provides a table of contents or index listing the current document.
- ``Last`: Indicates that the current document is a part of a series, and that the last document in the series is the referenced document.
- ``Licence`: Indicates that the main content of the current document is covered by the copyright license described by the referenced document.
- ``Manifest`: Imports or links to an application manifest.
- ``Me`: Indicates that the current document represents the person who owns the linked content.
- ``Modulepreload`: Specifies that the user agent must preemptively fetch the module script and store it in the document's module map for later evaluation. Optionally, the module's dependencies can be fetched as well.
- ``Next`: Indicates that the current document is a part of a series, and that the next document in the series is the referenced document.
- ``Nofollow`: Indicates that the current document's original author or publisher does not endorse the referenced document.
- ``Noopener`: Instructs the browser to open the link without granting the new browsing context access to the document that opened it.
- ``Noreferrer`: Requires that the user agent not send an HTTP Referer (sic) header if the user follows the hyperlink.
- ``Opener`: Creates an auxiliary browsing context if the hyperlink would otherwise create a top-level traversable with a non-auxiliary browsing context (i.e., has "\_blank" as target attribute value).
- ``Pingback`: Gives the address of the pingback server that handles pingbacks to the current document.
- ``Preconnect`: Specifies that the user agent should preemptively connect to the target resource's origin.
- ``Prefetch`: Specifies that the target resource should be preemptively cached.
- ``Preload`: Specifies that the user agent must preemptively fetch and cache the target resource for current navigation according to the potential destination given by the as attribute (and the priority associated with the corresponding destination).
- ``Prerender`: Specifies that the user agent should preemptively fetch the target resource and process it in a way that helps deliver a faster response in the future.
- ``Prev`: Indicates that the current document is a part of a series, and that the previous document in the series is the referenced document.
- ``Search`: Gives a link to a resource that can be used to search through the current document and its related pages.
- ``Stylesheet`: Imports a stylesheet.
- ``Sidebar`: Specifies that the referenced document, if retrieved, is intended to be shown in the browser's sidebar (if it has one).
- ``Tag`: Gives a tag (identified by the given address) that applies to the current document.
- ``Up`: Provides a link to a document giving the context for the current document.
```ocaml
type mediadesc_token = [ 
  | `All
  | `Aural
  | `Braille
  | `Embossed
  | `Handheld
  | `Print
  | `Projection
  | `Screen
  | `Speech
  | `Tty
  | `Tv
  | `Raw_mediadesc of string
 ]
```
```ocaml
type mediadesc = mediadesc_token list
```
The MediaDesc attribute is a comma-separated list of media descriptors. The following is a list of recognized media descriptors:

- ``Screen`: For non-paged computer screens.
- ``TTY`: For media using a fixed-pitch character grid (like teletypes, terminals, or devices with limited display capabilities).
- ``TV`: For TV-type devices (low resolution, limited scrollability).
- ``Projection`: For projectors.
- ``Handheld`: For handheld devices (small screen, limited bandwidth).
- ``Print`: For paged and for documents viewed on screen in print preview mode.
- ``Braille`: For braille tactile feedback devices.
- ``Aural`: For speech synthesizers.
- ``All`: For speech synthesizers.
- ``Raw_mediadesc`: For more complex (untyped) media descriptors.
```ocaml
type float_number = float
```
```ocaml
type pixels = int
```
The value is an integer that represents the number of pixels of the canvas (screen, paper). Thus, the value `"50"` means fifty pixels. For normative information about the definition of a pixel, please consult CSS2.

see [http://www.w3.org/TR/1998/REC-CSS2-19980512](http://www.w3.org/TR/1998/REC-CSS2-19980512) CSS2
```ocaml
type script_ = string
```
Script data can be the content of the `"script"` element and the value of intrinsic event attributes. User agents must not evaluate script data as HTML markup but instead must pass it on as data to a script engine.

The case-sensitivity of script data depends on the scripting language.

Please note that script data that is element content may not contain character references, but script data that is the value of an attribute may contain them.

```ocaml
type text = string
```
Arbitrary textual data, likely meant to be human-readable.


### Core

```ocaml
type i18n = [ 
  | `XML_lang
  | `Lang
 ]
```
```ocaml
type core = [ 
  | `Accesskey
  | `Class
  | `Contenteditable
  | `Contextmenu
  | `Dir
  | `Draggable
  | `Hidden
  | `Id
  | i18n
  | `Spellcheck
  | `Style_Attr
  | `Tabindex
  | `Translate
  | `Title
  | `User_data
  | `XMLns
 ]
```

### Events

```ocaml
type events = [ 
  | `OnAbort
  | `OnBlur
  | `OnCanPlay
  | `OnCanPlayThrough
  | `OnChange
  | `OnClick
  | `OnClose
  | `OnContextMenu
  | `OnDblClick
  | `OnDrag
  | `OnDragEnd
  | `OnDragEnter
  | `OnDragLeave
  | `OnDragOver
  | `OnDragStart
  | `OnDrop
  | `OnDurationChange
  | `OnEmptied
  | `OnEnded
  | `OnError
  | `OnFocus
  | `OnFormChange
  | `OnFormInput
  | `OnInput
  | `OnInvalid
  | `OnMouseDown
  | `OnMouseUp
  | `OnMouseOver
  | `OnMouseMove
  | `OnMouseOut
  | `OnMouseWheel
  | `OnPause
  | `OnPlay
  | `OnPlaying
  | `OnProgress
  | `OnRateChange
  | `OnReadyStateChange
  | `OnScroll
  | `OnSeeked
  | `OnSeeking
  | `OnSelect
  | `OnShow
  | `OnStalled
  | `OnSubmit
  | `OnSuspend
  | `OnTimeUpdate
  | `OnTouchStart
  | `OnTouchEnd
  | `OnTouchMove
  | `OnTouchCancel
  | `OnVolumeChange
  | `OnWaiting
  | `OnKeyPress
  | `OnKeyDown
  | `OnKeyUp
  | `OnLoad
  | `OnLoadedData
  | `OnLoadedMetaData
  | `OnLoadStart
 ]
```
Javascript events


### ARIA

```ocaml
type aria = [ 
  | `Role
  | `Aria
 ]
```
```ocaml
type common = [ 
  | core
  | i18n
  | events
  | aria
 ]
```
Common attributes


## Categories of HTML elements

These category are mainly subdivised in

- interactive,
- phrasing,
- flow5, these categories may overlap
```ocaml
type heading = [ 
  | `H1
  | `H2
  | `H3
  | `H4
  | `H5
  | `H6
  | `Hgroup
 ]
```
```ocaml
type sectioning = [ 
  | `Section
  | `Nav
  | `Aside
  | `Article
 ]
```
```ocaml
type resetable = [ 
  | `Textarea
  | `Select
  | `Output
  | `Keygen
  | `Input
 ]
```
```ocaml
type submitable = [ 
  | `Textarea
  | `Select
  | `Keygen
  | `Input
  | `Button
 ]
```
```ocaml
type labelable = [ 
  | resetable
  | `Progress
  | `Meter
  | `Button
 ]
```
```ocaml
type labelable_without_interactive = [ 
  | `Progress
  | `Meter
 ]
```
```ocaml
type formatblock = [ 
  | heading
  | sectioning
  | `Pre
  | `P
  | `Header
  | `Footer
  | `Div
  | `Blockquote
  | `Address
 ]
```
```ocaml
type sectionningroot = [ 
  | `Td
  | `Figure
  | `Fieldset
  | `Details
  | `Body
  | `Blockquote
  | `Dialog
 ]
```
```ocaml
type listed = [ 
  | resetable
  | submitable
  | `Fieldset
 ]
```
```ocaml
type formassociated = [ 
  | listed
  | `Progress
  | `Meter
  | `Label
 ]
```
```ocaml
type subresource_integrity = [ 
  | `Crossorigin
  | `Integrity
 ]
```
see [https://developer.mozilla.org/en-US/docs/Web/Security/Subresource\_Integrity](https://developer.mozilla.org/en-US/docs/Web/Security/Subresource_Integrity) 
```ocaml
type (+'interactive, +'noscript, +'regular, +'media) transparent = [ 
  | `A of 'interactive
  | `Noscript of 'noscript
  | `Canvas of 'regular
  | `Map of 'regular
  | `Ins of 'regular
  | `Del of 'regular
  | `Object of 'regular
  | `Object_interactive of 'regular
  | `Audio_interactive of 'media
  | `Video_interactive of 'media
  | `Audio of 'media
  | `Video of 'media
 ]
```
Transparent elements. Such elements have a part of they children in their data and behaves like them. We could do something like `a: 'a elt list -> 'a elt` but the information about the node name would be forgotten and would allow things like that : `p [a [a []]]`. This system allow to build non-conforming terms such as `a [a []]` but when passed to a standard element (such as `p`), it will yield an error. Exception to that : if you embdedd the element in another transparent (of an another kind) : `p [noscript (a [a []])]` will be correctly typed.

```ocaml
type (+'noscript, +'regular, +'media) transparent_without_interactive = [ 
  | `Noscript of 'noscript
  | `Ins of 'regular
  | `Del of 'regular
  | `Object of 'regular
  | `Canvas of 'regular
  | `Map of 'regular
  | `Audio of 'media
  | `Video of 'media
 ]
```
```ocaml
type (+'interactive, +'regular, +'media) transparent_without_noscript = [ 
  | `A of 'interactive
  | `Ins of 'regular
  | `Del of 'regular
  | `Canvas of 'regular
  | `Map of 'regular
  | `Object of 'regular
  | `Object_interactive of 'regular
  | `Video of 'media
  | `Audio of 'media
  | `Video_interactive of 'media
  | `Audio_interactive of 'media
 ]
```
```ocaml
type (+'interactive, +'noscript, +'regular) transparent_without_media = [ 
  | `A of 'interactive
  | `Noscript of 'noscript
  | `Ins of 'regular
  | `Del of 'regular
  | `Map of 'regular
  | `Canvas of 'regular
  | `Object of 'regular
  | `Object_interactive of 'regular
 ]
```
```ocaml
type metadata_without_title = [ 
  | `Template
  | `Style
  | `Script
  | `Noscript of [ `Meta | `Link | `Style ]
  | `Meta
  | `Link
  | `Command
  | `Base
 ]
```
Metadata without title

```ocaml
type metadata = [ 
  | metadata_without_title
  | `Title
 ]
```
Metadata contents. Used specially in \<head\>

Interactive contents : contents that require user-interaction (Forms, link, etc.)

```ocaml
type core_interactive = [ 
  | `Textarea
  | `Select
  | `Menu
  | `Label
  | `Keygen
  | `Input
  | `Img_interactive
  | `Iframe
  | `Embed
  | `Details
  | `Button
 ]
```
Core element types are element types without transparent.

```ocaml
type interactive = [ 
  | core_interactive
  | (interactive, interactive, interactive) transparent_without_interactive
 ]
```
```ocaml
type core_phrasing = [ 
  | labelable
  | submitable
  | `Wbr
  | `Var
  | `U
  | `Svg
  | `Time
  | `Template
  | `Sup
  | `Sub
  | `Strong
  | `Span
  | `Small
  | `Script
  | `Samp
  | `Ruby
  | `Q
  | `Mark
  | `Label
  | `Kbd
  | `Iframe
  | `I
  | `Embed
  | `Em
  | `Dfn
  | `Datalist
  | `Command
  | `Code
  | `Cite
  | `Br
  | `Bdo
  | `B
  | `Abbr
  | `Img
  | `Img_interactive
  | `Picture
  | `PCDATA
 ]
```
Phrasing contents is inline contents : bold text, span, and so on.

```ocaml
type core_phrasing_without_noscript = [ 
  | labelable
  | submitable
  | `Wbr
  | `Var
  | `U
  | `Time
  | `Template
  | `Sup
  | `Sub
  | `Svg
  | `Strong
  | `Span
  | `Small
  | `Script
  | `Samp
  | `Ruby
  | `Q
  | `Mark
  | `Label
  | `Kbd
  | `Iframe
  | `I
  | `Embed
  | `Em
  | `Dfn
  | `Datalist
  | `Command
  | `Code
  | `Cite
  | `Br
  | `Bdo
  | `Img
  | `Img_interactive
  | `Picture
  | `B
  | `Abbr
  | `PCDATA
 ]
```
```ocaml
type core_phrasing_without_interactive = [ 
  | labelable_without_interactive
  | `Wbr
  | `Var
  | `U
  | `Time
  | `Template
  | `Sup
  | `Sub
  | `Strong
  | `Span
  | `Small
  | `Script
  | `Svg
  | `Samp
  | `Ruby
  | `Q
  | `Mark
  | `Kbd
  | `Img
  | `Picture
  | `I
  | `Em
  | `Dfn
  | `Datalist
  | `Command
  | `Code
  | `Cite
  | `Br
  | `Bdo
  | `B
  | `Abbr
  | `PCDATA
 ]
```
```ocaml
type core_phrasing_without_media = [ 
  | labelable
  | submitable
  | `Wbr
  | `Var
  | `U
  | `Time
  | `Template
  | `Svg
  | `Sup
  | `Sub
  | `Strong
  | `Span
  | `Small
  | `Script
  | `Samp
  | `Ruby
  | `Q
  | `Mark
  | `Label
  | `Kbd
  | `Img
  | `Img_interactive
  | `Picture
  | `Iframe
  | `I
  | `Embed
  | `Em
  | `Dfn
  | `Datalist
  | `Command
  | `Code
  | `Cite
  | `Br
  | `Bdo
  | `B
  | `Abbr
  | `PCDATA
 ]
```
```ocaml
type phrasing_without_noscript =
  (phrasing_without_interactive, phrasing, phrasing_without_media)
    transparent_without_noscript
```
```ocaml
and phrasing_without_media = [ 
  | core_phrasing_without_media
  | (phrasing_without_interactive, phrasing_without_noscript, phrasing)
    transparent_without_media
 ]
```
```ocaml
and phrasing_without_interactive = [ 
  | core_phrasing_without_interactive
  | (phrasing_without_noscript, phrasing, phrasing_without_media)
    transparent_without_interactive
 ]
```
```ocaml
and phrasing = [ 
  | (phrasing_without_interactive,
    phrasing_without_noscript,
    phrasing,
    phrasing_without_media)
    transparent
  | core_phrasing
 ]
```
```ocaml
type (+'a, +'b) between_phrasing_and_phrasing_without_interactive =
  [< core_phrasing
  | ([< phrasing_without_interactive ] as 'b,
      phrasing_without_noscript,
      phrasing,
      phrasing_without_media)
      transparent Abbr B Bdo Br Canvas Cite Code Command Datalist Del Dfn Em I Img Picture Ins Kbd Map Mark Meter Noscript Object PCDATA Progress Q Ruby Samp Script Small Span Strong Sub Sup Svg Template Time U Var Wbr ] as 'a
```
```ocaml
type phrasing_without_dfn = [ 
  | labelable
  | submitable
  | `Wbr
  | `Var
  | `U
  | `Time
  | `Template
  | `Sup
  | `Sub
  | `Strong
  | `Span
  | `Small
  | `Script
  | `Samp
  | `Ruby
  | `Q
  | `Mark
  | `Label
  | `Img
  | `Img_interactive
  | `Picture
  | `Kbd
  | `I
  | `Em
  | `Datalist
  | `Command
  | `Code
  | `Cite
  | `Br
  | `Bdo
  | `B
  | `Abbr
  | `PCDATA
  | (phrasing_without_interactive,
    phrasing_without_noscript,
    phrasing_without_dfn,
    phrasing_without_media)
    transparent
 ]
```
Phrasing without the interactive markups

```ocaml
type phrasing_without_label = [ 
  | labelable
  | submitable
  | `Wbr
  | `Var
  | `U
  | `Time
  | `Template
  | `Sup
  | `Sub
  | `Strong
  | `Span
  | `Img
  | `Img_interactive
  | `Picture
  | `Small
  | `Script
  | `Samp
  | `Ruby
  | `Q
  | `Mark
  | `Kbd
  | `I
  | `Em
  | `Dfn
  | `Datalist
  | `Command
  | `Code
  | `Cite
  | `Br
  | `Bdo
  | `B
  | `Abbr
  | `PCDATA
  | (phrasing_without_interactive,
    phrasing_without_noscript,
    phrasing_without_label,
    phrasing_without_media)
    transparent
 ]
```
```ocaml
type phrasing_without_progress = [ 
  | resetable
  | submitable
  | `Wbr
  | `Var
  | `U
  | `Time
  | `Template
  | `Sup
  | `Sub
  | `Strong
  | `Span
  | `Small
  | `Script
  | `Samp
  | `Img
  | `Img_interactive
  | `Picture
  | `Ruby
  | `Q
  | `Meter
  | `Mark
  | `Label
  | `Kbd
  | `I
  | `Em
  | `Dfn
  | `Datalist
  | `Command
  | `Code
  | `Cite
  | `Button
  | `Br
  | `Bdo
  | `B
  | `Abbr
  | `PCDATA
  | (phrasing_without_interactive,
    phrasing_without_noscript,
    phrasing_without_progress,
    phrasing_without_media)
    transparent
 ]
```
```ocaml
type phrasing_without_time = [ 
  | labelable
  | submitable
  | `Wbr
  | `Var
  | `U
  | `Template
  | `Sup
  | `Sub
  | `Strong
  | `Img
  | `Img_interactive
  | `Picture
  | `Span
  | `Small
  | `Script
  | `Samp
  | `Ruby
  | `Q
  | `Mark
  | `Label
  | `Kbd
  | `I
  | `Em
  | `Dfn
  | `Datalist
  | `Command
  | `Code
  | `Cite
  | `Br
  | `Bdo
  | `B
  | `Abbr
  | `PCDATA
  | (phrasing_without_interactive,
    phrasing_without_noscript,
    phrasing_without_time,
    phrasing_without_media)
    transparent
 ]
```
```ocaml
type phrasing_without_meter = [ 
  | submitable
  | resetable
  | `Progress
  | `Button
  | `Wbr
  | `Var
  | `U
  | `Time
  | `Template
  | `Sup
  | `Img
  | `Img_interactive
  | `Picture
  | `Sub
  | `Strong
  | `Span
  | `Small
  | `Script
  | `Samp
  | `Ruby
  | `Q
  | `Mark
  | `Label
  | `Kbd
  | `I
  | `Em
  | `Dfn
  | `Datalist
  | `Command
  | `Code
  | `Cite
  | `Br
  | `Bdo
  | `B
  | `Abbr
  | `PCDATA
  | (phrasing_without_interactive,
    phrasing_without_noscript,
    phrasing_without_meter,
    phrasing_without_media)
    transparent
 ]
```
```ocaml
type core_flow5 = [ 
  | core_phrasing
  | formassociated
  | formatblock
  | `Ul
  | `Table
  | `Style
  | `Ol
  | `Menu
  | `Hr
  | `Form
  | `Figure
  | `Dl
  | `Details
  | `Main
  | `Dialog
 ]
```
```ocaml
type core_flow5_without_interactive = [ 
  | core_phrasing_without_interactive
  | formassociated
  | formatblock
  | `Ul
  | `Table
  | `Style
  | `Ol
  | `Menu
  | `Hr
  | `Form
  | `Figure
  | `Dl
  | `Main
  | `Dialog
 ]
```
```ocaml
type core_flow5_without_noscript = [ 
  | core_phrasing_without_noscript
  | formassociated
  | formatblock
  | `Ul
  | `Table
  | `Style
  | `Ol
  | `Menu
  | `Hr
  | `Form
  | `Figure
  | `Dl
  | `Details
  | `Main
  | `Dialog
 ]
```
```ocaml
type core_flow5_without_media = [ 
  | core_phrasing_without_media
  | formassociated
  | formatblock
  | `Ul
  | `Table
  | `Style
  | `Ol
  | `Menu
  | `Hr
  | `Form
  | `Figure
  | `Dl
  | `Details
  | `Main
  | `Dialog
 ]
```
```ocaml
type flow5_without_interactive = [ 
  | core_flow5_without_interactive
  | (flow5_without_noscript, flow5, flow5_without_media)
    transparent_without_interactive
 ]
```
```ocaml
and flow5_without_noscript = [ 
  | core_flow5_without_noscript
  | (flow5_without_interactive, flow5, flow5_without_media)
    transparent_without_noscript
 ]
```
```ocaml
and flow5_without_media = [ 
  | core_flow5_without_media
  | (flow5_without_interactive, flow5_without_noscript, flow5)
    transparent_without_media
 ]
```
```ocaml
and flow5 = [ 
  | core_flow5
  | (flow5_without_interactive, flow5_without_noscript, flow5, flow5_without_media)
    transparent
 ]
```
```ocaml
type flow5_without_table = [ 
  | core_phrasing
  | formassociated
  | formatblock
  | `Ul
  | `Style
  | `Ol
  | `Menu
  | `Hr
  | `Form
  | `Figure
  | `Dl
  | `Details
  | `Main
  | `Dialog
  | (flow5_without_interactive, flow5_without_noscript, flow5, flow5_without_media)
    transparent
 ]
```
```ocaml
type flow5_without_interactive_header_footer = [ 
  | heading
  | sectioning
  | `Pre
  | `P
  | `Div
  | `Blockquote
  | `Address
  | core_phrasing_without_interactive
  | formassociated
  | `Ul
  | `Table
  | `Style
  | `Ol
  | `Menu
  | `Hr
  | `Form
  | `Figure
  | `Dl
  | `Main
  | `Dialog
  | (flow5_without_noscript, flow5, flow5_without_media)
    transparent_without_interactive
 ]
```
```ocaml
type flow5_without_header_footer = [ 
  | heading
  | sectioning
  | `Pre
  | `P
  | `Div
  | `Blockquote
  | `Address
  | core_phrasing
  | formassociated
  | `Ul
  | `Table
  | `Style
  | `Ol
  | `Menu
  | `Hr
  | `Form
  | `Figure
  | `Dl
  | `Details
  | `Main
  | `Dialog
  | (flow5_without_interactive_header_footer,
    flow5_without_noscript,
    flow5,
    flow5_without_media)
    transparent
 ]
```
```ocaml
type +'a between_flow5_and_flow5_without_interactive_header_footer =
  [< flow5 Abbr Address Article Aside Audio B Bdo Blockquote Br Button Canvas Cite Code Command Datalist Del Dfn Dialog Div Dl Em Fieldset Figure Form H1 H2 H3 H4 H5 H6 Hgroup Hr I Img Picture Input Ins Kbd Keygen Label Map Mark Menu Meter Nav Noscript Object Ol Output P PCDATA Pre Progress Q Ruby Samp Script Section Select Small Span Strong Style Sub Sup Svg Table Template Textarea Time U Ul Var Video Wbr ] as 'a
```
```ocaml
type (+'a, +'b) between_flow5_and_flow5_without_header_footer =
  [< core_flow5
  | ([< flow5_without_interactive ] as 'b,
      flow5_without_noscript,
      'a,
      flow5_without_media)
      transparent A Abbr Address Article Aside Audio Audio_interactive B Bdo Blockquote Br Button Canvas Cite Code Command Datalist Del Details Dfn Dialog Div Dl Em Embed Fieldset Figure Form H1 H2 H3 H4 H5 H6 Hgroup Hr I Iframe Img Img_interactive Picture Input Ins Kbd Keygen Label Map Mark Menu Meter Nav Noscript Object Object_interactive Ol Output P PCDATA Pre Progress Q Ruby Samp Script Section Select Small Span Strong Style Sub Sup Svg Table Template Textarea Time U Ul Var Video Video_interactive Wbr ] as 'a
```
```ocaml
type flow5_without_form = [ 
  | core_phrasing
  | formassociated
  | formatblock
  | `Ul
  | `Table
  | `Style
  | `Ol
  | `Menu
  | `Hr
  | `Figure
  | `Dl
  | `Details
  | `Main
  | `Dialog
  | (flow5_without_interactive, flow5_without_noscript, flow5, flow5_without_media)
    transparent
 ]
```
```ocaml
type flow5_without_sectioning_heading_header_footer_address = [ 
  | core_phrasing
  | formassociated
  | `Pre
  | `P
  | `Div
  | `Blockquote
  | `Ul
  | `Table
  | `Style
  | `Ol
  | `Menu
  | `Hr
  | `Form
  | `Figure
  | `Dl
  | `Details
  | `Main
  | `Dialog
  | (flow5_without_interactive, flow5_without_noscript, flow5, flow5_without_media)
    transparent
 ]
```
```ocaml
type flow5_without_sectioning_heading_header_footer = [ 
  | flow5_without_sectioning_heading_header_footer_address
  | `Address
 ]
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
```ocaml
type notag
```
```ocaml
type no_attribute_allowed
```
```ocaml
type noattrib = [ 
  | `No_attribute_allowed of no_attribute_allowed
 ]
```
```ocaml
type html = [ 
  | `Html
 ]
```
```ocaml
type html_content_fun = [ 
  | `Head
  | `Body
 ]
```
```ocaml
type html_content = html_content_fun
```
```ocaml
type html_attrib = [ 
  | common
  | `Manifest
 ]
```
```ocaml
type head = [ 
  | `Head
 ]
```
```ocaml
type head_content = [ 
  | metadata
 ]
```
```ocaml
type head_content_fun = [ 
  | metadata_without_title
 ]
```
```ocaml
type head_attrib = [ 
  | common
 ]
```
```ocaml
type body = [ 
  | `Body
 ]
```
```ocaml
type body_attrib = [ 
  | common
  | `OnAfterPrint
  | `OnBeforePrint
  | `OnBeforeUnload
  | `OnHashChange
  | `OnMessage
  | `OnOffLine
  | `OnOnLine
  | `OnPageHide
  | `OnPageShow
  | `OnPopState
  | `OnRedo
  | `OnResize
  | `OnStorage
  | `OnUndo
  | `OnUnload
 ]
```
```ocaml
type body_content = flow5
```
```ocaml
type body_content_fun = flow5
```
```ocaml
type svg = [ 
  | `Svg
 ]
```
```ocaml
type svg_content = Svg_types.svg_content
```
```ocaml
type svg_attrib = Svg_types.svg_attr
```
```ocaml
type base = [ 
  | `Base
 ]
```
```ocaml
type base_content = notag
```
```ocaml
type base_content_fun = notag
```
```ocaml
type base_attrib = [ 
  | common
  | `Href
  | `Target
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
type title_content_fun = [ 
  | `PCDATA
 ]
```
```ocaml
type title_attrib = noattrib
```
```ocaml
type footer = [ 
  | `Footer
 ]
```
```ocaml
type footer_content = [ 
  | flow5_without_header_footer
 ]
```
```ocaml
type footer_content_fun = [ 
  | flow5_without_header_footer
 ]
```
```ocaml
type footer_attrib = [ 
  | common
 ]
```
```ocaml
type header = [ 
  | `Header
 ]
```
```ocaml
type header_content = [ 
  | flow5_without_header_footer
 ]
```
```ocaml
type header_content_fun = [ 
  | flow5_without_header_footer
 ]
```
```ocaml
type header_attrib = [ 
  | common
 ]
```
```ocaml
type section = [ 
  | `Section
 ]
```
```ocaml
type section_content = [ 
  | flow5
 ]
```
```ocaml
type section_content_fun = [ 
  | flow5
 ]
```
```ocaml
type section_attrib = [ 
  | common
 ]
```
```ocaml
type nav = [ 
  | `Nav
 ]
```
```ocaml
type nav_content = [ 
  | flow5
 ]
```
```ocaml
type nav_content_fun = [ 
  | flow5
 ]
```
```ocaml
type nav_attrib = [ 
  | common
 ]
```
```ocaml
type h1 = [ 
  | `H1
 ]
```
```ocaml
type h1_content = [ 
  | phrasing
 ]
```
```ocaml
type h1_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type h1_attrib = [ 
  | common
 ]
```
```ocaml
type h2 = [ 
  | `H2
 ]
```
```ocaml
type h2_content = [ 
  | phrasing
 ]
```
```ocaml
type h2_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type h2_attrib = [ 
  | common
 ]
```
```ocaml
type h3 = [ 
  | `H3
 ]
```
```ocaml
type h3_content = [ 
  | phrasing
 ]
```
```ocaml
type h3_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type h3_attrib = [ 
  | common
 ]
```
```ocaml
type h4 = [ 
  | `H4
 ]
```
```ocaml
type h4_content = [ 
  | phrasing
 ]
```
```ocaml
type h4_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type h4_attrib = [ 
  | common
 ]
```
```ocaml
type h5 = [ 
  | `H5
 ]
```
```ocaml
type h5_content = [ 
  | phrasing
 ]
```
```ocaml
type h5_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type h5_attrib = [ 
  | common
 ]
```
```ocaml
type h6 = [ 
  | `H6
 ]
```
```ocaml
type h6_content = [ 
  | phrasing
 ]
```
```ocaml
type h6_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type h6_attrib = [ 
  | common
 ]
```
```ocaml
type hgroup = [ 
  | `Hgroup
 ]
```
```ocaml
type hgroup_content = [ 
  | `H1
  | `H2
  | `H3
  | `H4
  | `H5
  | `H6
 ]
```
```ocaml
type hgroup_content_fun = [ 
  | `H1
  | `H2
  | `H3
  | `H4
  | `H5
  | `H6
 ]
```
```ocaml
type hgroup_attrib = [ 
  | common
 ]
```
```ocaml
type address = [ 
  | `Address
 ]
```
```ocaml
type address_content = [ 
  | flow5_without_sectioning_heading_header_footer_address
 ]
```
```ocaml
type address_content_fun = [ 
  | flow5_without_sectioning_heading_header_footer_address
 ]
```
```ocaml
type address_attrib = [ 
  | common
 ]
```
```ocaml
type article = [ 
  | `Article
 ]
```
```ocaml
type article_content = [ 
  | flow5
 ]
```
```ocaml
type article_content_fun = [ 
  | flow5
 ]
```
```ocaml
type article_attrib = [ 
  | common
 ]
```
```ocaml
type aside = [ 
  | `Aside
 ]
```
```ocaml
type aside_content = [ 
  | flow5
 ]
```
```ocaml
type aside_content_fun = [ 
  | flow5
 ]
```
```ocaml
type aside_attrib = [ 
  | common
 ]
```
```ocaml
type main = [ 
  | `Main
 ]
```
```ocaml
type main_content = [ 
  | flow5
 ]
```
```ocaml
type main_content_fun = [ 
  | flow5
 ]
```
```ocaml
type main_attrib = [ 
  | common
 ]
```
```ocaml
type p = [ 
  | `P
 ]
```
```ocaml
type p_content = [ 
  | phrasing
 ]
```
```ocaml
type p_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type p_attrib = [ 
  | common
 ]
```
```ocaml
type pre = [ 
  | `Pre
 ]
```
```ocaml
type pre_content = [ 
  | phrasing
 ]
```
```ocaml
type pre_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type pre_attrib = [ 
  | common
 ]
```
```ocaml
type blockquote = [ 
  | `Blockquote
 ]
```
```ocaml
type blockquote_content = [ 
  | flow5
 ]
```
```ocaml
type blockquote_content_fun = [ 
  | flow5
 ]
```
```ocaml
type blockquote_attrib = [ 
  | common
  | `Cite
 ]
```
```ocaml
type dialog = [ 
  | `Dialog
 ]
```
```ocaml
type dialog_content = [ 
  | flow5
 ]
```
```ocaml
type dialog_content_fun = [ 
  | flow5
 ]
```
```ocaml
type dialog_attrib = [ 
  | common
  | `Open
 ]
```
```ocaml
type div = [ 
  | `Div
 ]
```
```ocaml
type div_content = [ 
  | flow5
 ]
```
```ocaml
type div_content_fun = [ 
  | flow5
 ]
```
```ocaml
type div_attrib = [ 
  | common
 ]
```
```ocaml
type ol = [ 
  | `Ol
 ]
```
```ocaml
type ol_content = [ 
  | `Li of [ common | `Int_Value ]
 ]
```
```ocaml
type ol_content_fun = [ 
  | `Li of [ common | `Int_Value ]
 ]
```
```ocaml
type ol_attrib = [ 
  | common
  | `Reversed
  | `Start
 ]
```
```ocaml
type li_content = [ 
  | flow5
 ]
```
```ocaml
type li_content_fun = [ 
  | flow5
 ]
```
```ocaml
type li_attrib = [ 
  | common
  | `Int_Value
 ]
```
```ocaml
type li = [ 
  | `Li of li_attrib
 ]
```
```ocaml
type ul = [ 
  | `Ul
 ]
```
```ocaml
type ul_content = [ 
  | `Li of [ li_attrib ]
 ]
```
```ocaml
type ul_content_fun = [ 
  | `Li of [ li_attrib ]
 ]
```
```ocaml
type ul_attrib = [ 
  | common
 ]
```
```ocaml
type dd = [ 
  | `Dd
 ]
```
```ocaml
type dd_content = [ 
  | flow5
 ]
```
```ocaml
type dd_content_fun = [ 
  | flow5
 ]
```
```ocaml
type dd_attrib = [ 
  | common
 ]
```
```ocaml
type dt = [ 
  | `Dt
 ]
```
```ocaml
type dt_content = [ 
  | flow5_without_sectioning_heading_header_footer
 ]
```
```ocaml
type dt_content_fun = [ 
  | flow5_without_sectioning_heading_header_footer
 ]
```
```ocaml
type dt_attrib = [ 
  | common
 ]
```
```ocaml
type dl = [ 
  | `Dl
 ]
```
```ocaml
type dl_content = [ 
  | `Dt
  | `Dd
 ]
```
```ocaml
type dl_content_fun = [ 
  | `Dt
  | `Dd
 ]
```
```ocaml
type dl_attrib = [ 
  | common
 ]
```
```ocaml
type figcaption = [ 
  | `Figcaption
 ]
```
```ocaml
type figcaption_content = [ 
  | flow5
 ]
```
```ocaml
type figcaption_content_fun = [ 
  | flow5
 ]
```
```ocaml
type figcaption_attrib = [ 
  | common
 ]
```
```ocaml
type figure = [ 
  | `Figure
 ]
```
```ocaml
type figure_content = [ 
  | flow5
 ]
```
```ocaml
type figure_content_fun = [ 
  | flow5
 ]
```
```ocaml
type figure_attrib = [ 
  | common
 ]
```
```ocaml
type rp = [ 
  | `Rp
 ]
```
```ocaml
type rp_content = [ 
  | phrasing
 ]
```
```ocaml
type rp_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type rp_attrib = [ 
  | common
 ]
```
```ocaml
type rt = [ 
  | `Rt
 ]
```
```ocaml
type rt_content = [ 
  | phrasing
 ]
```
```ocaml
type rt_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type rt_attrib = [ 
  | common
 ]
```
```ocaml
type ruby = [ 
  | `Ruby
 ]
```
```ocaml
type ruby_content = [ 
  | phrasing
  | rp
  | rt
 ]
```
```ocaml
type ruby_content_fun = [ 
  | phrasing
  | rp
  | rt
 ]
```
```ocaml
type ruby_attrib = [ 
  | common
 ]
```
```ocaml
type hr = [ 
  | `Hr
 ]
```
```ocaml
type hr_content = notag
```
```ocaml
type hr_content_fun = notag
```
```ocaml
type hr_attrib = [ 
  | common
 ]
```
```ocaml
type b = [ 
  | `B
 ]
```
```ocaml
type b_content = [ 
  | phrasing
 ]
```
```ocaml
type b_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type b_attrib = [ 
  | common
 ]
```
```ocaml
type i = [ 
  | `I
 ]
```
```ocaml
type i_content = [ 
  | phrasing
 ]
```
```ocaml
type i_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type i_attrib = [ 
  | common
 ]
```
```ocaml
type u = [ 
  | `U
 ]
```
```ocaml
type u_content = [ 
  | phrasing
 ]
```
```ocaml
type u_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type u_attrib = [ 
  | common
 ]
```
```ocaml
type small = [ 
  | `Small
 ]
```
```ocaml
type small_content = [ 
  | phrasing
 ]
```
```ocaml
type small_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type small_attrib = [ 
  | common
 ]
```
```ocaml
type sub = [ 
  | `Sub
 ]
```
```ocaml
type sub_content = [ 
  | phrasing
 ]
```
```ocaml
type sub_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type sub_attrib = [ 
  | common
 ]
```
```ocaml
type sup = [ 
  | `Sup
 ]
```
```ocaml
type sup_content = [ 
  | phrasing
 ]
```
```ocaml
type sup_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type sup_attrib = [ 
  | common
 ]
```
```ocaml
type mark = [ 
  | `Mark
 ]
```
```ocaml
type mark_content = [ 
  | phrasing
 ]
```
```ocaml
type mark_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type mark_attrib = [ 
  | common
 ]
```
```ocaml
type wbr = [ 
  | `Wbr
 ]
```
```ocaml
type wbr_content = notag
```
```ocaml
type wbr_content_fun = notag
```
```ocaml
type wbr_attrib = [ 
  | common
 ]
```
```ocaml
type bdo = [ 
  | `Bdo
 ]
```
```ocaml
type bdo_content = [ 
  | phrasing
 ]
```
```ocaml
type bdo_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type bdo_attrib = [ 
  | common
 ]
```
```ocaml
type abbr = [ 
  | `Abbr
 ]
```
```ocaml
type abbr_content = [ 
  | phrasing
 ]
```
```ocaml
type abbr_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type abbr_attrib = [ 
  | common
 ]
```
```ocaml
type br = [ 
  | `Br
 ]
```
```ocaml
type br_content = notag
```
```ocaml
type br_content_fun = notag
```
```ocaml
type br_attrib = [ 
  | common
 ]
```
```ocaml
type cite = [ 
  | `Cite
 ]
```
```ocaml
type cite_content = [ 
  | phrasing
 ]
```
```ocaml
type cite_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type cite_attrib = [ 
  | common
 ]
```
```ocaml
type code = [ 
  | `Code
 ]
```
```ocaml
type code_content = [ 
  | phrasing
 ]
```
```ocaml
type code_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type code_attrib = [ 
  | common
 ]
```
```ocaml
type dfn = [ 
  | `Dfn
 ]
```
```ocaml
type dfn_content = [ 
  | phrasing_without_dfn
 ]
```
```ocaml
type dfn_content_fun = [ 
  | phrasing_without_dfn
 ]
```
```ocaml
type dfn_attrib = [ 
  | common
 ]
```
```ocaml
type em = [ 
  | `Em
 ]
```
```ocaml
type em_content = [ 
  | phrasing
 ]
```
```ocaml
type em_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type em_attrib = [ 
  | common
 ]
```
```ocaml
type kbd = [ 
  | `Kbd
 ]
```
```ocaml
type kbd_content = [ 
  | phrasing
 ]
```
```ocaml
type kbd_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type kbd_attrib = [ 
  | common
 ]
```
```ocaml
type q = [ 
  | `Q
 ]
```
```ocaml
type q_content = [ 
  | phrasing
 ]
```
```ocaml
type q_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type q_attrib = [ 
  | common
  | `Cite
 ]
```
```ocaml
type samp = [ 
  | `Samp
 ]
```
```ocaml
type samp_content = [ 
  | phrasing
 ]
```
```ocaml
type samp_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type samp_attrib = [ 
  | common
 ]
```
```ocaml
type span = [ 
  | `Span
 ]
```
```ocaml
type span_content = [ 
  | phrasing
 ]
```
```ocaml
type span_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type span_attrib = [ 
  | common
 ]
```
```ocaml
type strong = [ 
  | `Strong
 ]
```
```ocaml
type strong_content = [ 
  | phrasing
 ]
```
```ocaml
type strong_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type strong_attrib = [ 
  | common
 ]
```
```ocaml
type time = [ 
  | `Time
 ]
```
```ocaml
type time_content = [ 
  | phrasing_without_time
 ]
```
```ocaml
type time_content_fun = [ 
  | phrasing_without_time
 ]
```
```ocaml
type time_attrib = [ 
  | common
  | `Datetime
  | `Pubdate
 ]
```
```ocaml
type var = [ 
  | `Var
 ]
```
```ocaml
type var_content = [ 
  | phrasing
 ]
```
```ocaml
type var_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type var_attrib = [ 
  | common
 ]
```
```ocaml
type a_content = flow5_without_interactive
```
```ocaml
type a_content_fun = flow5_without_interactive
```
```ocaml
type 'a a = [ 
  | `A of 'a
 ]
```
```ocaml
type a_ = [ 
  | `A of a_content
 ]
```
```ocaml
type a_attrib = [ 
  | common
  | `Href
  | `Hreflang
  | `Media
  | `Rel
  | `Target
  | `Mime_type
  | `Download
 ]
```
```ocaml
type 'a del = [ 
  | `Del of 'a
 ]
```
```ocaml
type del_content = flow5
```
```ocaml
type del_ = del_content del
```
```ocaml
type del_content_fun = flow5
```
```ocaml
type del_attrib = [ 
  | common
  | `Cite
  | `Datetime
 ]
```
```ocaml
type 'a ins = [ 
  | `Ins of 'a
 ]
```
```ocaml
type ins_content = flow5
```
```ocaml
type ins_ = ins_content ins
```
```ocaml
type ins_content_fun = flow5
```
```ocaml
type ins_attrib = [ 
  | common
  | `Cite
  | `Datetime
 ]
```
```ocaml
type iframe = [ 
  | `Iframe
 ]
```
```ocaml
type iframe_content = [ 
  | `PCDATA
 ]
```
```ocaml
type iframe_content_fun = [ 
  | `PCDATA
 ]
```
```ocaml
type iframe_attrib = [ 
  | common
  | `Allowfullscreen
  | `Allowpaymentrequest
  | `Src
  | `Name
  | `Sandbox
  | `Seamless
  | `Width
  | `Height
  | `Referrerpolicy
 ]
```
```ocaml
type object__content = [ 
  | flow5
  | `Param
 ]
```
```ocaml
type object__content_fun = flow5
```
```ocaml
type 'a object_ = [ 
  | `Object of 'a
  | `Object_interactive of 'a
 ]
```
```ocaml
type object__ = object__content object_
```
```ocaml
type object__attrib = [ 
  | common
  | `Data
  | `Form
  | `Mime_type
  | `Height
  | `Width
  | `Name
  | `Usemap
 ]
```
```ocaml
type param = [ 
  | `Param
 ]
```
```ocaml
type param_content = notag
```
```ocaml
type param_content_fun = notag
```
```ocaml
type param_attrib = [ 
  | common
  | `Name
  | `Text_Value
 ]
```
```ocaml
type embed = [ 
  | `Embed
 ]
```
```ocaml
type embed_content = notag
```
```ocaml
type embed_content_fun = notag
```
```ocaml
type embed_attrib = [ 
  | common
  | `Src
  | `Height
  | `Mime_type
  | `Width
 ]
```
```ocaml
type img = [ 
  | `Img
 ]
```
```ocaml
type img_interactive = [ 
  | `Img
  | `Img_interactive
 ]
```
```ocaml
type img_content = notag
```
```ocaml
type img_content_fun = notag
```
```ocaml
type img_attrib = [ 
  | common
  | `Height
  | `Ismap
  | `Width
  | `Srcset
  | `Img_sizes
 ]
```
```ocaml
type media_attrib = [ 
  | `Crossorigin
  | `Preload
  | `Autoplay
  | `Mediagroup
  | `Loop
  | `Muted
  | `Controls
 ]
```
```ocaml
type 'a audio = [ 
  | `Audio of 'a
 ]
```
```ocaml
type 'a audio_interactive = [ 
  | `Audio of 'a
  | `Audio_interactive of 'a
 ]
```
```ocaml
type audio_content = flow5_without_media
```
```ocaml
type audio_ = audio_content audio
```
```ocaml
type audio_content_fun = flow5_without_media
```
```ocaml
type audio_attrib = [ 
  | common
  | media_attrib
 ]
```
```ocaml
type 'a video = [ 
  | `Video of 'a
 ]
```
```ocaml
type 'a video_interactive = [ 
  | `Video of 'a
  | `Video_interactive of 'a
 ]
```
```ocaml
type video_content = flow5_without_media
```
```ocaml
type video_ = video_content video
```
```ocaml
type video_content_fun = flow5_without_media
```
```ocaml
type video_attrib = [ 
  | common
  | media_attrib
  | `Poster
  | `Width
  | `Height
 ]
```
```ocaml
type 'a canvas = [ 
  | `Canvas of 'a
 ]
```
```ocaml
type canvas_content = flow5
```
```ocaml
type canvas_ = canvas_content canvas
```
```ocaml
type canvas_content_fun = flow5
```
```ocaml
type canvas_attrib = [ 
  | common
  | `Width
  | `Height
 ]
```
```ocaml
type source = [ 
  | `Source
 ]
```
```ocaml
type source_content = notag
```
```ocaml
type source_content_fun = notag
```
```ocaml
type source_attrib = [ 
  | common
  | `Src
  | `Srcset
  | `Mime_type
  | `Media
 ]
```
```ocaml
type area = [ 
  | `Area
 ]
```
```ocaml
type area_content = notag
```
```ocaml
type area_content_fun = notag
```
```ocaml
type area_attrib = [ 
  | common
  | `Alt
  | `Coords
  | `Shape
  | `Target
  | `Rel
  | `Media
  | `Hreflang
  | `Mime_type
  | `Download
 ]
```
```ocaml
type 'a map = [ 
  | `Map of 'a
 ]
```
```ocaml
type map_content = flow5
```
```ocaml
type map_ = map_content map
```
```ocaml
type map_content_fun = flow5
```
```ocaml
type map_attrib = [ 
  | common
  | `Name
 ]
```
```ocaml
type caption = [ 
  | `Caption
 ]
```
```ocaml
type caption_content = [ 
  | flow5_without_table
 ]
```
```ocaml
type caption_content_fun = [ 
  | flow5_without_table
 ]
```
```ocaml
type caption_attrib = [ 
  | common
 ]
```
```ocaml
type table = [ 
  | `Table
 ]
```
```ocaml
type table_content = [ 
  | `Tr
 ]
```
```ocaml
type table_content_fun = [ 
  | `Tr
 ]
```
```ocaml
type table_attrib = [ 
  | common
  | `Summary
 ]
```
```ocaml
type tablex = [ 
  | `Table
 ]
```
```ocaml
type tablex_content = [ 
  | `Tbody
 ]
```
```ocaml
type tablex_content_fun = [ 
  | `Tbody
 ]
```
```ocaml
type tablex_attrib = [ 
  | common
  | `Summary
 ]
```
```ocaml
type colgroup = [ 
  | `Colgroup
 ]
```
```ocaml
type colgroup_content = [ 
  | `Col
 ]
```
```ocaml
type colgroup_content_fun = [ 
  | `Col
 ]
```
```ocaml
type colgroup_attrib = [ 
  | common
  | `Span
 ]
```
```ocaml
type col = [ 
  | `Col
 ]
```
```ocaml
type col_content = notag
```
```ocaml
type col_content_fun = notag
```
```ocaml
type col_attrib = [ 
  | common
  | `Span
 ]
```
```ocaml
type thead = [ 
  | `Thead
 ]
```
```ocaml
type thead_content = [ 
  | `Tr
 ]
```
```ocaml
type thead_content_fun = [ 
  | `Tr
 ]
```
```ocaml
type thead_attrib = [ 
  | common
 ]
```
```ocaml
type tbody = [ 
  | `Tbody
 ]
```
```ocaml
type tbody_content = [ 
  | `Tr
 ]
```
```ocaml
type tbody_content_fun = [ 
  | `Tr
 ]
```
```ocaml
type tbody_attrib = [ 
  | common
 ]
```
```ocaml
type tfoot = [ 
  | `Tfoot
 ]
```
```ocaml
type tfoot_content = [ 
  | `Tr
 ]
```
```ocaml
type tfoot_content_fun = [ 
  | `Tr
 ]
```
```ocaml
type tfoot_attrib = [ 
  | common
 ]
```
```ocaml
type td = [ 
  | `Td
 ]
```
```ocaml
type td_content = [ 
  | flow5
 ]
```
```ocaml
type td_content_fun = [ 
  | flow5
 ]
```
```ocaml
type td_attrib = [ 
  | common
  | `Colspan
  | `Headers
  | `Rowspan
 ]
```
```ocaml
type th = [ 
  | `Th
 ]
```
```ocaml
type th_content = [ 
  | flow5
 ]
```
```ocaml
type th_content_fun = [ 
  | flow5
 ]
```
```ocaml
type th_attrib = [ 
  | common
  | `Colspan
  | `Headers
  | `Rowspan
  | `Scope
 ]
```
```ocaml
type tr = [ 
  | `Tr
 ]
```
```ocaml
type tr_content = [ 
  | `Td
  | `Th
 ]
```
```ocaml
type tr_content_fun = [ 
  | `Td
  | `Th
 ]
```
```ocaml
type tr_attrib = [ 
  | common
 ]
```
```ocaml
type form = [ 
  | `Form
 ]
```
```ocaml
type form_content = [ 
  | flow5_without_form
 ]
```
```ocaml
type form_content_fun = [ 
  | flow5_without_form
 ]
```
```ocaml
type form_attrib = [ 
  | common
  | `Accept_charset
  | `Action
  | `Enctype
  | `Method
  | `Name
  | `Target
  | `Autocomplete
  | `Novalidate
 ]
```
```ocaml
type fieldset = [ 
  | `Fieldset
 ]
```
```ocaml
type fieldset_content = [ 
  | flow5
 ]
```
```ocaml
type fieldset_content_fun = [ 
  | flow5
 ]
```
```ocaml
type fieldset_attrib = [ 
  | common
  | `Disabled
  | `Form
  | `Name
 ]
```
```ocaml
type legend = [ 
  | `Legend
 ]
```
```ocaml
type legend_content = [ 
  | phrasing
 ]
```
```ocaml
type legend_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type legend_attrib = [ 
  | common
 ]
```
```ocaml
type label = [ 
  | `Label
 ]
```
```ocaml
type label_content = [ 
  | phrasing_without_label
 ]
```
```ocaml
type label_content_fun = [ 
  | phrasing_without_label
 ]
```
```ocaml
type label_attrib = [ 
  | common
  | `Label_for
  | `Form
 ]
```
```ocaml
type input = [ 
  | `Input
 ]
```
```ocaml
type input_content = notag
```
```ocaml
type input_content_fun = notag
```
```ocaml
type input_attrib = [ 
  | common
  | `Accept
  | `Alt
  | `Autocomplete
  | `Autofocus
  | `Checked
  | `Disabled
  | `Form
  | `Formaction
  | `Formenctype
  | `Formmethod
  | `Method
  | `Formnovalidate
  | `Formtarget
  | `Height
  | `List
  | `Input_Max
  | `Maxlength
  | `Minlength
  | `Input_Min
  | `Multiple
  | `Name
  | `Pattern
  | `Placeholder
  | `ReadOnly
  | `Required
  | `Size
  | `Src
  | `Step
  | `Input_Type
  | `Value
  | `Width
  | `Inputmode
 ]
```
```ocaml
type textarea = [ 
  | `Textarea
 ]
```
```ocaml
type textarea_attrib = [ 
  | common
  | `Autofocus
  | `Disabled
  | `Form
  | `Maxlength
  | `Minlength
  | `Name
  | `Placeholder
  | `ReadOnly
  | `Required
  | `Wrap
  | `Rows
  | `Cols
 ]
```
```ocaml
type textarea_content = [ 
  | `PCDATA
 ]
```
```ocaml
type textarea_content_fun = textarea_content
```
```ocaml
type button = [ 
  | `Button
 ]
```
```ocaml
type button_content = [ 
  | phrasing_without_interactive
 ]
```
```ocaml
type button_content_fun = [ 
  | phrasing_without_interactive
 ]
```
```ocaml
type button_attrib = [ 
  | common
  | `Autofocus
  | `Disabled
  | `Form
  | `Formaction
  | `Formenctype
  | `Formmethod
  | `Method
  | `Formnovalidate
  | `Formtarget
  | `Name
  | `Text_Value
  | `Button_Type
 ]
```
```ocaml
type select = [ 
  | `Select
 ]
```
```ocaml
type select_content = [ 
  | `Optgroup
  | `Option
 ]
```
```ocaml
type select_content_fun = [ 
  | `Optgroup
  | `Option
 ]
```
```ocaml
type select_attrib = [ 
  | common
  | `Autofocus
  | `Multiple
  | `Name
  | `Size
  | `Form
  | `Disabled
  | `Required
 ]
```
```ocaml
type datalist = [ 
  | `Datalist
 ]
```
```ocaml
type datalist_content = notag
```
```ocaml
type datalist_content_fun = notag
```
```ocaml
type datalist_attrib = [ 
  | common
 ]
```
```ocaml
type optgroup = [ 
  | `Optgroup
 ]
```
```ocaml
type optgroup_content = [ 
  | `Option
 ]
```
```ocaml
type optgroup_content_fun = [ 
  | `Option
 ]
```
```ocaml
type optgroup_attrib = [ 
  | common
  | `Disabled
  | `Label
 ]
```
```ocaml
type option_attrib = [ 
  | common
  | `Selected
  | `Text_Value
  | `Disabled
  | `Label
  | `Value
 ]
```
```ocaml
type selectoption = [ 
  | `Option
 ]
```
```ocaml
type option_content_fun = [ 
  | `PCDATA
 ]
```
```ocaml
type option_content = [ 
  | `PCDATA
 ]
```
```ocaml
type keygen = [ 
  | `Keygen
 ]
```
```ocaml
type keygen_content = notag
```
```ocaml
type keygen_content_fun = notag
```
```ocaml
type keygen_attrib = [ 
  | common
  | `Autofocus
  | `Challenge
  | `Disabled
  | `Form
  | `Keytype
  | `Name
 ]
```
```ocaml
type progress = [ 
  | `Progress
 ]
```
```ocaml
type progress_content = [ 
  | phrasing_without_progress
 ]
```
```ocaml
type progress_content_fun = [ 
  | phrasing_without_progress
 ]
```
```ocaml
type progress_attrib = [ 
  | common
  | `Float_Value
  | `Max
  | `Form
 ]
```
```ocaml
type meter = [ 
  | `Meter
 ]
```
```ocaml
type meter_content = [ 
  | phrasing_without_meter
 ]
```
```ocaml
type meter_content_fun = [ 
  | phrasing_without_meter
 ]
```
```ocaml
type meter_attrib = [ 
  | common
  | `Float_Value
  | `Min
  | `Max
  | `Low
  | `High
  | `Optimum
  | `Form
 ]
```
```ocaml
type output_elt = [ 
  | `Output
 ]
```
```ocaml
type output_elt_content = [ 
  | phrasing
 ]
```
```ocaml
type output_elt_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type output_elt_attrib = [ 
  | common
  | `Form
  | `Output_for
  | `Name
 ]
```
```ocaml
type details = [ 
  | `Details
 ]
```
```ocaml
type details_content = [ 
  | flow5
 ]
```
```ocaml
type details_content_fun = [ 
  | flow5
 ]
```
```ocaml
type details_attrib = [ 
  | common
  | `Open
 ]
```
```ocaml
type summary = [ 
  | `Summary
 ]
```
```ocaml
type summary_content = [ 
  | phrasing
 ]
```
```ocaml
type summary_content_fun = [ 
  | phrasing
 ]
```
```ocaml
type summary_attrib = [ 
  | common
 ]
```
```ocaml
type command = [ 
  | `Command
 ]
```
```ocaml
type command_content = notag
```
```ocaml
type command_content_fun = notag
```
```ocaml
type command_attrib = [ 
  | common
  | `Icon
  | `Disabled
  | `Checked
  | `Radiogroup
  | `Command_Type
 ]
```
```ocaml
type menu = [ 
  | `Menu
 ]
```
```ocaml
type menu_content = notag
```
```ocaml
type menu_content_fun = notag
```
```ocaml
type menu_attrib = [ 
  | common
  | `Label
  | `Menu_Type
 ]
```
```ocaml
type noscript = [ 
  | `Noscript of flow5_without_noscript
 ]
```
```ocaml
type noscript_content = flow5_without_noscript
```
```ocaml
type noscript_content_fun = flow5_without_noscript
```
```ocaml
type noscript_attrib = [ 
  | common
 ]
```
```ocaml
type meta = [ 
  | `Meta
 ]
```
```ocaml
type meta_content = notag
```
```ocaml
type meta_content_fun = notag
```
```ocaml
type meta_attrib = [ 
  | common
  | `Http_equiv
  | `Name
  | `Content
  | `Charset
  | `Property
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
type style_content_fun = [ 
  | `PCDATA
 ]
```
```ocaml
type style_attrib = [ 
  | common
  | `Media
  | `Mime_type
  | `Scoped
 ]
```
```ocaml
type script = [ 
  | `Script
 ]
```
```ocaml
type script_attrib = [ 
  | common
  | subresource_integrity
  | `Async
  | `Charset
  | `Src
  | `Defer
  | `Script_type
 ]
```
```ocaml
type script_content = [ 
  | `PCDATA
 ]
```
```ocaml
type script_content_fun = [ 
  | `PCDATA
 ]
```
```ocaml
type template = [ 
  | `Template
 ]
```
```ocaml
type template_attrib = [ 
  | common
 ]
```
```ocaml
type template_content = [ 
  | flow5
 ]
```
```ocaml
type template_content_fun = [ 
  | flow5
 ]
```
```ocaml
type link = [ 
  | `Link
 ]
```
```ocaml
type link_content = notag
```
```ocaml
type link_content_fun = notag
```
```ocaml
type link_attrib = [ 
  | common
  | subresource_integrity
  | `Hreflang
  | `Media
  | `Rel
  | `Href
  | `Sizes
  | `Mime_type
 ]
```
```ocaml
type picture = [ 
  | `Picture
 ]
```
```ocaml
type picture_content = [ 
  | source
  | script
  | template
 ]
```
```ocaml
type picture_content_fun = [ 
  | source
  | script
  | template
 ]
```
```ocaml
type picture_attrib = [ 
  | common
 ]
```
```ocaml
type referrerpolicy = [ 
  | `Empty
  | `No_referrer
  | `No_referrer_when_downgrade
  | `Origin
  | `Origin_when_cross_origin
  | `Same_origin
  | `Strict_origin
  | `Strict_origin_when_cross_origin
  | `Unsafe_url
 ]
```
```ocaml
type big_variant = [ 
  | `W3_org_1999_xhtml
  | `Default
  | `Preserve
  | `Selected
  | `Get
  | `Post
  | `Checked
  | `Disabled
  | `ReadOnly
  | `Async
  | `Autofocus
  | `Autoplay
  | `Muted
  | `Anonymous
  | `Use_credentials
  | `Controls
  | `Ltr
  | `Rtl
  | `Formnovalidate
  | `Hidden
  | `Ismap
  | `Loop
  | `Novalidate
  | `Open
  | `Audio
  | `Metadata
  | `None
  | `Pubdate
  | `Required
  | `Reversed
  | `Scoped
  | `Seamless
  | `Hard
  | `Soft
  | `Multiple
  | `Checkbox
  | `Command
  | `Radio
  | `Context
  | `Toolbar
  | `Char
  | `Justify
  | `Left
  | `Right
  | `Col
  | `Colgroup
  | `Row
  | `Rowgroup
  | `All
  | `Cols
  | `Groups
  | `None
  | `Rows
  | `Rect
  | `Circle
  | `Poly
  | `Default
  | `One
  | `Zero
  | `Auto
  | `No
  | `Yes
  | `Defer
  | `Verbatim
  | `Latin
  | `Latin_name
  | `Latin_prose
  | `Full_width_latin
  | `Kana
  | `Katakana
  | `Numeric
  | `Tel
  | `Email
  | `Url
  | `Text
  | `Decimal
  | `Search
 ]
```
```ocaml
type sandbox_token = [ 
  | `Allow_forms
  | `Allow_pointer_lock
  | `Allow_popups
  | `Allow_top_navigation
  | `Allow_same_origin
  | `Allow_script
 ]
```
```ocaml
type input_type = [ 
  | `Button
  | `Checkbox
  | `Color
  | `Date
  | `Datetime
  | `Datetime_local
  | `Email
  | `File
  | `Hidden
  | `Image
  | `Month
  | `Number
  | `Password
  | `Radio
  | `Range
  | `Reset
  | `Search
  | `Submit
  | `Tel
  | `Text
  | `Time
  | `Url
  | `Week
 ]
```
```ocaml
type script_type = [ 
  | `Javascript
  | `Module
  | `Mime of string
 ]
```
```ocaml
type autocomplete_option = [ 
  | `On
  | `Off
  | `Tokens of string list
 ]
```