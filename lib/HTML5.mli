
module type T = HTML5_sigs.HTML5(XML)(SVG.M).T

module M : T

module P : XML_sigs.TypedSimplePrinter(XML)(M).T

module MakePrinter(O : XML_sigs.Output) : XML_sigs.TypedPrinter(XML)(M)(O).T
