
module Make 
    (S : Svg_sigs.NoWrap)
    (H : Html_sigs.NoWrap
     with module Xml = S.Xml
      and module Svg := S) : sig

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
  (** [element ppf x] pretty prints the tyxml element [x] on [ppf].

      Warning: For pretty printing purposes, [x] is always considered of 
      width 0! To print text, use the {!string} combinator.
      To print HTML of which you know the size, use {!element_sized}.
  *)

  val element_sized : (int * out) t
  (** [element_sized ppf (i,x)] pretty prints the tyxml element [x] 
      with size [i] on [ppf].

      See the documentation of {!element} for details.
  *)
  
  val elements : out list t
  (** [elements ppf l] prints a list of tyxml elements [l].

      See the documentation of {!element} for details.
  *)

  val tagf :
    (out list -> out) -> formatter ->
    ('a, formatter, unit, unit) format4 -> 'a

  val u : 
    ?a:[< Html_types.u_attrib ] H.attrib list ->
    'a t -> 'a t
  val b : 
    ?a:[< Html_types.u_attrib ] H.attrib list ->
    'a t -> 'a t
  val span : 
    ?a:[< Html_types.u_attrib ] H.attrib list ->
    'a t -> 'a t
  
  val star :
    ('a, Html_types.phrasing, Html_types.phrasing) H.star ->
    ?a:'a H.attrib list -> 'a t -> 'a t
  val starl :
    ('a, Html_types.phrasing, Html_types.phrasing) H.star ->
    ?a:'a H.attrib list -> ?sep:unit t -> 'a t -> 'a list t

  val wrapped : (out list -> out) -> 'a t -> 'a t
  
  (** {2:formatter Formatter manipulation} *)

  val make : unit -> formatter
  (** Create a new formatter. *)

  val flush : formatter -> out list
  
  (** {2:fmt {!Fmt} API} *)
  include Fmt.S with type formatter := formatter and type 'a t := 'a t

end
