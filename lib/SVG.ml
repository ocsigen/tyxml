(* sVG.ml module
   based on http://www.w3.org/TR/SVG *)

module type T = SVG_sigs.SVG(XML).T

module M = SVG_f.Make(XML)

module P = XML_print.MakeTypedSimple(XML)(M)

module MakePrinter = XML_print.MakeTyped(XML)(M)
