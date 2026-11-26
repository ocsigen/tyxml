(** Typed implementation for HTML, SVG and XML

    This is the natural implementation of the TyXML combinators based on an XML
    data-structure. Other implementations are available, see
    {{!page-"index"}the manual} for details. *)

module Html = Tyxml_html
(** Typesafe constructors and printers for HTML documents.

    @see <http://www.w3.org/TR/html/> W3C Recommendation *)

module Svg = Tyxml_svg
(** Typesafe constructors and printers for Svg documents.

    @see <http://www.w3.org/TR/SVG> W3C Recommendation *)

module Xml = Tyxml_xml
(** Basic functions for construction and manipulation of XML tree. *)

module Html5 = Tyxml_html
[@@ocaml.deprecated "Use Tyxml.Html"]
(** Deprecated alias for {!Html}.
    @deprecated "Use Html" *)
