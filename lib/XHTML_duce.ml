
module type T = XML_sigs_duce.TypedXML
                with type doc = XHTML_types_duce.html
		 and type elt = {{ XHTML_types_duce.block
				 | XHTML_types_duce.form
				 | XHTML_types_duce.misc }}

module M_01_00 = struct

  module Info = XHTML.M_01_00.Info

  type doc = {{ XHTML_types_duce.html }}
  type elt = {{ XHTML_types_duce.block
	      | XHTML_types_duce.form
	      | XHTML_types_duce.misc }}
end

module M_01_00_compat = struct

  module Info = XHTML.M_01_00_compat.Info

  type doc = {{ XHTML_types_duce.html }}
  type elt = {{ XHTML_types_duce.block
	      | XHTML_types_duce.form
	      | XHTML_types_duce.misc }}

end

module M = M_01_00_compat

module P_01_00 = struct

  module P = XML_print_duce.MakeTypedRaw(M_01_00)

  type doc = M.doc
  type elt = M.elt

  let print_list = (P.print_list :>output:(string -> unit) ->
         ?encode:(string -> string) ->
         elt list -> unit)
  let print = (P.print :> output:(string -> unit) ->
         ?encode:(string -> string) -> ?advert:string -> doc -> unit)

end

module P_01_00_compat = struct

  module P = XML_print_duce.MakeTypedRaw(M_01_00_compat)

  type doc = M.doc
  type elt = M.elt

  let print_list = (P.print_list :>output:(string -> unit) ->
         ?encode:(string -> string) ->
         elt list -> unit)
  let print = (P.print :> output:(string -> unit) ->
         ?encode:(string -> string) -> ?advert:string -> doc -> unit)

end

module P = P_01_00_compat
