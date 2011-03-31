
module type T = SVG_sigs.SVG(XML).T

module M : T

module P : XML_sigs.TypedSimplePrinter(XML)(M).T

module MakePrinter(O : XML_sigs.Output) : XML_sigs.TypedPrinter(XML)(M)(O).T
