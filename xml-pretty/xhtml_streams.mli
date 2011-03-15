
module type Stream = sig
  type s
  type m
  val empty: unit -> m Lazy.t
  val concat: m Lazy.t -> m Lazy.t -> m Lazy.t
  val put: string -> m Lazy.t
  val make: m Lazy.t -> s
end

module type Printer = sig

  include Xhtml_format.Info
  type s

  (** Ocsigen's compact printer for XML. [html_compat] is an option to set
      if you want to print with a syntax closer to html (not xml).
   *)
  val xhtml_stream :
      ?version:doctypes ->
      ?width:int -> ?encode:(string -> string) ->
      ?html_compat:bool ->
      [ `Html ] elt -> s

(** Ocsigen's compact printer for XML portions.
    [html_compat] is an option to set
    if you want to print with a syntax closer to html (not xml). *)
  val xhtml_list_stream :
      ?width:int -> ?encode:(string -> string) ->
      ?html_compat:bool ->
      'a elt list -> s

end
