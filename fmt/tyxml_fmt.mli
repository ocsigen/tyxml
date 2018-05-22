
module Make (H : Html_sigs.NoWrap) : sig

  type out = Html_types.phrasing H.elt
  (** Type of the output of the printer. *)

  type 'a attribs = 'a H.attrib list
  (** Attributes of Html elements. *)
  
  type formatter
  (** The type of tyxml-aware formatters. *)

  type 'a t = formatter -> 'a -> unit
  
  val kpr : 
    (out list -> 'a) ->
    ('b, formatter, unit, 'a) format4 -> 'b
  val pr : 
    ('a, formatter, unit, out list) format4 -> 'a

  (** {2:html HTML printers} *)
    
  val element : out t
  val elements : out list t

  val u : 
    ?a:[< Html_types.u_attrib ] H.attrib list ->
    ?sep:unit t -> 'a t -> 'a list t
  val b : 
    ?a:[< Html_types.u_attrib ] H.attrib list ->
    ?sep:unit t -> 'a t -> 'a list t
  val span : 
    ?a:[< Html_types.u_attrib ] H.attrib list ->
    ?sep:unit t -> 'a t -> 'a list t
  
  val nullary : 
    ('a, Html_types.phrasing) H.nullary -> 'a H.attrib list ->
    formatter -> unit
  val unary : 
    ('a, 'b, Html_types.phrasing) H.unary -> 'a H.attrib list ->
    ('b H.elt) t
  val star :
    ('a, Html_types.phrasing, Html_types.phrasing) H.star ->
    ?a:'a H.attrib list -> ?sep:unit t -> 'a t -> 'a list t

  (** {2:formatter Formatter manipulation} *)

  val make : unit -> formatter
  (** Create a new formatter. *)

  val flush : formatter -> out list
  
  (** {2:fmt {!Fmt} API} *)
  include Fmt.S with type formatter := formatter and type 'a t := 'a t

end
