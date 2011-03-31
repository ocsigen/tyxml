
module type T = XHTML_sigs.XHTML(XML).T
module type T_01_00 = XHTML_sigs.XHTML(XML).T_01_00
module type T_01_01 = XHTML_sigs.XHTML(XML).T_01_01

module M : T
module M_01_00 : T_01_00
module M_01_01 : T_01_01
module M_01_01_compat : T_01_01

module P : XML_sigs.TypedSimplePrinter(XML)(M).T
module P_01_00 : XML_sigs.TypedSimplePrinter(XML)(M_01_00).T
module P_01_01 : XML_sigs.TypedSimplePrinter(XML)(M_01_01).T
module P_01_01_compat : XML_sigs.TypedSimplePrinter(XML)(M_01_01_compat).T

module MakePrinter(O : XML_sigs.Output) : XML_sigs.TypedPrinter(XML)(M)(O).T
module MakePrinter_01_00(O : XML_sigs.Output) : XML_sigs.TypedPrinter(XML)(M_01_00)(O).T
module MakePrinter_01_01(O : XML_sigs.Output) : XML_sigs.TypedPrinter(XML)(M_01_01)(O).T
module MakePrinter_01_01_compat(O : XML_sigs.Output) : XML_sigs.TypedPrinter(XML)(M_01_01_compat)(O).T
