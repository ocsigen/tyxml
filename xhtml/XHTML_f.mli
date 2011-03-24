
module Make(XML : XML_defs.T)
     : XHTML_defs.T with type raw_xml_elt       = XML.elt
                     and type raw_xml_attrib    = XML.attrib
                     and type raw_xml_event     = XML.event
module Make_01_01(XML : XML_defs.T)
     : XHTML_defs.T with type raw_xml_elt       = XML.elt
                     and type raw_xml_attrib    = XML.attrib
                     and type raw_xml_event     = XML.event
module Make_01_00(XML : XML_defs.T)
     : XHTML_defs.T with type raw_xml_elt       = XML.elt
                     and type raw_xml_attrib    = XML.attrib
                     and type raw_xml_event     = XML.event

