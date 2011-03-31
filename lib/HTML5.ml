
module type T = HTML5_sigs.HTML5(XML)(SVG.M).T

module M = HTML5_f.Make(XML)(SVG.M)

module P = XML_print.MakeTypedSimple(XML)(M)

module MakePrinter = XML_print.MakeTyped(XML)(M)
