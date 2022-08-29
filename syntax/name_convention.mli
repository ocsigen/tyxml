(** Gives the tyxml names for HTML elements and attributes. *)

val to_ocaml : string -> string
(** The transformations are the following:

    - Valid letters in OCaml identifiers are kept.
    - Everything else is turn into an underscore '_'. *)

val ident : string -> string
(** Turn the given element name into a valid identifier.

    Follow the [to_ocaml] convention and lowercase the first letter. *)

val attrib : string -> string
(** Turn the given attribute name into a valid identifier.

    Follow the [to_ocaml] convention and add ["a_"] at the beginning. *)

val polyvar : string -> string
(** Turn the given name into a valid Polymorphic variant name.

    Follow the [to_ocaml] convention, uppercase the first letter and add ["`"]. *)
