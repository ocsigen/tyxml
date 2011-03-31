
module type T = XHTML_sigs.XHTML(XML).T
module type T_01_00 = XHTML_sigs.XHTML(XML).T_01_00
module type T_01_01 = XHTML_sigs.XHTML(XML).T_01_01

module M = XHTML_f.Make(XML)
module M_01_00 = XHTML_f.Make_01_00(XML)
module M_01_01 = XHTML_f.Make_01_01(XML)
module M_01_01_compat = XHTML_f.Make_01_01(XML)

module P = XML_print.MakeTypedSimple(XML)(M)
module P_01_00 = XML_print.MakeTypedSimple(XML)(M_01_00)
module P_01_01 = XML_print.MakeTypedSimple(XML)(M_01_01)
module P_01_01_compat = XML_print.MakeTypedSimple(XML)(M_01_01)

open XML_sigs

module MakePrinter = XML_print.MakeTyped(XML)(M)
module MakePrinter_01_01 = XML_print.MakeTyped(XML)(M_01_01)
module MakePrinter_01_00 = XML_print.MakeTyped(XML)(M_01_00)
module MakePrinter_01_00_compat = XML_print.MakeTyped(XML)(M_01_00)
