
(** Type instantiations for SVG *)

(** This module defines basic data types for data, attributes
    and element occuring in SVG documents.
    It is based on the specification available at http://www.w3.org/TR/SVG/.

    This module is experimental, it may lack of some attributes,
    and the interface is very low level and do not take deeply into account
    the needs of SVG elements. *)

module Make(XML: XML_defs.T) : SVG_defs.T with type raw_xml_elt = XML.elt
                                           and type raw_xml_attrib = XML.attrib

