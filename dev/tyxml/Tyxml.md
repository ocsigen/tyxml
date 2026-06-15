
# Module `Tyxml`

Typed implementation for HTML, SVG and XML

This is the natural implementation of the TyXML combinators based on an XML data-structure. `
    Other implementations are available, see {{!page-"intro"}the manual} for details. `

```ocaml
module Html = Tyxml_html
```
Typesafe constructors and printers for HTML documents.

```ocaml
module Svg = Tyxml_svg
```
Typesafe constructors and printers for Svg documents.

```ocaml
module Xml = Tyxml_xml
```
Basic functions for construction and manipulation of XML tree.

```ocaml
module Html5 = Tyxml_html
```
Deprecated alias for [`Html`](./Tyxml_html.md).
