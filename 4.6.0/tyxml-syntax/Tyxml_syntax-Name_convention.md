
# Module `Tyxml_syntax.Name_convention`

Gives the tyxml names for HTML elements and attributes.

```ocaml
val to_ocaml : string -> string
```
The transformations are the following:

- Valid letters in OCaml identifiers are kept.
- Everything else is turn into an underscore '\_'.
```ocaml
val ident : string -> string
```
Turn the given element name into a valid identifier.

Follow the `to_ocaml` convention and lowercase the first letter.

```ocaml
val attrib : string -> string
```
Turn the given attribute name into a valid identifier.

Follow the `to_ocaml` convention and add `"a_"` at the beginning.

```ocaml
val polyvar : string -> string
```
Turn the given name into a valid Polymorphic variant name.

Follow the `to_ocaml` convention, uppercase the first letter and add `"`"`.
