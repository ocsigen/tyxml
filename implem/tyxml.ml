(** Typed implementation for HTML, SVG and XML

    This is the natural implementation of the TyXML combinators
    based on an XML data-structure.
    {%
    Other implementations are available, see <<a_manual chapter="intro"|the manual>> for details. %}
*)

(** Typesafe constructors and printers for HTML documents.

    @see <http://www.w3.org/TR/html5/> W3C Recommendation *)
module Html5 = Tyxml_html5

(** Typesafe constructors and printers for Svg documents.

    @see <http://www.w3.org/TR/SVG> W3C Recommendation *)
module Svg = Tyxml_svg


(** Basic functions for construction and manipulation of XML tree. *)
module Xml = Tyxml_xml
