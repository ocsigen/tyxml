

module type Printer = sig

  val print_list:
    output:(string -> unit) ->
      ?encode:(string -> string) ->
        Ocamlduce.Load.anyxml list -> unit

end

module type TypedXML = sig

    module Info : XML_sigs.Info
    type elt
    type doc

end

module type RawTypedPrinter = sig
  val print_list:
    output:(string -> unit) ->
    ?encode:(string -> string) ->
    {{ Ocamlduce.Load.anyxml }} list -> unit
  val print:
    output:(string -> unit) ->
    ?encode:(string -> string) ->
    ?advert:string-> {{ Ocamlduce.Load.anyxml }} -> unit
end

module TypedPrinter(TypedXML : TypedXML) = struct

  module type T = sig
    type elt = TypedXML.elt
    type doc = TypedXML.doc
    val print_list:
      output:(string -> unit) ->
        ?encode:(string -> string) ->
          elt list -> unit
    val print:
      output:(string -> unit) ->
        ?encode:(string -> string) ->
          ?advert:string-> doc -> unit
  end

end
