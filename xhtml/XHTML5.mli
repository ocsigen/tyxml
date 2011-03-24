
module M
     : XHTML5_defs.T with type raw_xml_elt       = XML.elt
                      and type raw_xml_attrib    = XML.attrib
                      and type raw_xml_event     = XML.event
                      and type 'a raw_svg_elt    = 'a SVG.M.elt
                      and type 'a raw_svg_attrib = 'a SVG.M.attrib

module M_05_00
     : XHTML5_defs.T_05_00 with type raw_xml_elt       = XML.elt
                            and type raw_xml_attrib    = XML.attrib
                            and type raw_xml_event     = XML.event
                            and type 'a raw_svg_elt    = 'a SVG.M.elt
                            and type 'a raw_svg_attrib = 'a SVG.M.attrib

