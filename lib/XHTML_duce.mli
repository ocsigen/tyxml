
module type T = XML_sigs_duce.TypedXML
                with type doc = XHTML_types_duce.html
		 and type elt = {{ XHTML_types_duce.block
				 | XHTML_types_duce.form
				 | XHTML_types_duce.misc }}

module M : T
module M_01_00 : T
module M_01_00_compat : T

module P : XML_sigs_duce.TypedPrinter(M).T
module P_01_00 : XML_sigs_duce.TypedPrinter(M_01_00).T
module P_01_00_compat : XML_sigs_duce.TypedPrinter(M_01_00_compat).T
