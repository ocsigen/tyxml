
module Make(XML : XML_sigs.Iterable) : sig

  open XML

  val amap : (ename -> attrib list -> attrib list) -> elt -> elt
(** Recursively edit attributes for the element and all its children. *)

  val amap1 : (ename -> attrib list -> attrib list) -> elt -> elt
(** Edit attributes only for one element. *)

(** The following can safely be exported by higher level libraries,
    because removing an attribute from a element is always legal. *)

  val rm_attrib : (aname -> bool) -> attrib list -> attrib list
  val rm_attrib_from_list : (aname -> bool) -> (string -> bool) -> attrib list -> attrib list

  val map_int_attrib :
      (aname -> bool) -> (int -> int) -> attrib list -> attrib list
  val map_string_attrib :
      (aname -> bool) -> (string -> string) -> attrib list -> attrib list
  val map_string_attrib_in_list :
      (aname -> bool) -> (string -> string) -> attrib list -> attrib list

(** Exporting the following by higher level libraries would drive
    a hole through a type system, because they allow to add {e any}
    attribute to {e any} element. *)

  val add_int_attrib : aname -> int -> attrib list -> attrib list
  val add_string_attrib : aname -> string -> attrib list -> attrib list
  val add_comma_sep_attrib : aname -> string -> attrib list -> attrib list
  val add_space_sep_attrib : aname -> string -> attrib list -> attrib list

  val fold : (unit -> 'a) -> (string -> 'a) -> (string -> 'a) -> (string -> 'a) ->
    (string -> 'a) -> (ename -> attrib list -> 'a) ->
      (ename -> attrib list -> 'a list -> 'a) ->
	elt -> 'a

(* (* is this AT ALL useful??? *)
   val foldx : (unit -> 'a) -> (string -> 'a) -> (string -> 'a) -> (string -> 'a) ->
   ('state -> ename -> attrib list -> 'a) ->
   ('state -> ename -> attrib list -> 'a list -> 'a) ->
   (ename -> attrib list -> 'state -> 'state) -> 'state -> elt -> 'a
 *)


  val all_entities : elt -> string list

  val translate :
      (ename -> attrib list -> elt) ->
	(ename -> attrib list -> elt list -> elt) ->
          ('state -> ename -> attrib list -> elt list) ->
            ('state -> ename -> attrib list -> elt list -> elt list) ->
              (ename -> attrib list -> 'state -> 'state) -> 'state -> elt -> elt

end
