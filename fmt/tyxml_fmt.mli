open Format

module Make 
    (S : Svg_sigs.NoWrap)
    (H : Html_sigs.NoWrap
     with module Xml = S.Xml
      and module Svg := S) : sig

  type out = Html_types.phrasing H.elt
  (** Type of the output of the printer. *)

  type 'a attribs = 'a H.attrib list
  (** Attributes of Html elements. *)
  
  val kpr : 
    (out list -> 'a) ->
    ('b, formatter, unit, 'a) format4 -> 'b
  val pr : 
    ('a, formatter, unit, out list) format4 -> 'a

  (** {2:html HTML printers} *)
    
  val element : out Fmt.t
  (** [element ppf x] pretty prints the tyxml element [x] on [ppf].

      Warning: For pretty printing purposes, [x] is always considered of 
      width 0! To print text, use the {!string} combinator.
      To print HTML of which you know the size, use {!element_sized}.
  *)

  val element_sized : (int * out) Fmt.t
  (** [element_sized ppf (i,x)] pretty prints the tyxml element [x] 
      with size [i] on [ppf].

      See the documentation of {!element} for details.
  *)
  
  val elements : out list Fmt.t
  (** [elements ppf l] prints a list of tyxml elements [l].

      See the documentation of {!element} for details.
  *)

  val tagf :
    (out list -> out) -> ('a, formatter, unit, formatter -> unit) format4 -> 'a

  val u : 
    ?a:[< Html_types.u_attrib ] H.attrib list ->
    ?sep:unit Fmt.t ->
    'a Fmt.t -> 'a list Fmt.t
  val b : 
    ?a:[< Html_types.b_attrib ] H.attrib list ->
    ?sep:unit Fmt.t ->
    'a Fmt.t -> 'a list Fmt.t
  val span : 
    ?a:[< Html_types.span_attrib ] H.attrib list ->
    ?sep:unit Fmt.t ->
    'a Fmt.t -> 'a list Fmt.t
  
  val star :
    ('a, Html_types.phrasing, Html_types.phrasing) H.star ->
    ?a:'a H.attrib list -> ?sep:unit Fmt.t -> 'e Fmt.t -> 'e list Fmt.t

  val wrapped : (out list -> out) -> 'a Fmt.t -> 'a Fmt.t
  
  (** {2:formatter Formatter manipulation} *)

  type formatter
  
  val make : unit -> formatter
  (** Create a new formatter. *)
  
  val flush : formatter -> out list

  val kpf : 
    (out list -> 'o) ->
    formatter -> ('a, Format.formatter, unit, 'o) format4 -> 'a

  val pf : formatter -> ('a, Format.formatter, unit, out list) format4 -> 'a
  
end
