
# Module `Xml_print`

Printing utilities.

This module contains various encoding functions that can be used with [`Tyxml.Html.pp`](./Tyxml_html.md#val-pp) and [`Tyxml.Svg.pp`](./Tyxml_svg.md#val-pp).

It also contains functors to create printers for your own XML data structure.


### Encoding functions

```ocaml
val encode_unsafe_char : string -> string
```
The encoder maps strings to HTML and *must* encode the unsafe characters `'<'`, `'>'`, `'"'`, `'&'` and the control characters 0-8, 11-12, 14-31, 127 to HTML entities. `encode_unsafe_char` is the default for `?encode` in `output` and `pretty_print` below. Other implementations are provided by the module `Netencoding` in the [OcamlNet](http://www.ocaml-programming.de/programming/ocamlnet.html) library, e.g.:

```ocaml
let encode = Netencoding.Html.encode ~in_enc:`Enc_iso88591 ~out_enc:`Enc_usascii ()
```
Where national characters are replaced by HTML entities. The user is of course free to write her own implementation.

see [http://www.ocaml-programming.de/programming/ocamlnet.html](http://www.ocaml-programming.de/programming/ocamlnet.html) OcamlNet
```ocaml
val encode_unsafe_char_and_at : string -> string
```
In addition, encode `"@"` as `"&#64;"` in the hope that this will fool simple minded email address harvesters.

```ocaml
module Utf8 : sig ... end
```
Utf8 normalizer and encoder for HTML.


### Utilities

```ocaml
val compose_decl : ?version:string -> ?encoding:string -> unit -> string
```
`encoding` is the name of the character encoding, e.g. `"US-ASCII"` or `"UTF-8"`

```ocaml
val compose_doctype : string -> string list -> string
```
```ocaml
val string_of_number : float -> string
```
Convert a float to a string using a compact representation compatible with the Javascript norm.

```ocaml
val pp_number : Stdlib.Format.formatter -> float -> unit
```
See [`string_of_number`](./#val-string_of_number).


### Formatter functors

```ocaml
module Make_typed_fmt
  (Xml : Xml_sigs.Iterable)
  (Typed_xml : Xml_sigs.Typed_xml with module Xml := Xml) : 
  Xml_sigs.Typed_pp
    with type 'a elt := 'a Typed_xml.elt
     and type doc := Typed_xml.doc
```
Printers for typed XML modules such as the one produced by [`Svg_f`](./Svg_f.md) and [`Html_f`](./Html_f.md).

```ocaml
module type TagList = sig ... end
```
List of tags that can be printed as empty tags: `<foo />`.

```ocaml
module Make_fmt
  (Xml : Xml_sigs.Iterable)
  (I : TagList) : 
  Xml_sigs.Pp with type elt := Xml.elt
```
Printers for raw XML modules.


### Deprecated functors

Use [`Make_fmt`](./Xml_print-Make_fmt.md) and [`Make_typed_fmt`](./Xml_print-Make_typed_fmt.md) instead.

```ocaml
module Make
  (Xml : Xml_sigs.Iterable)
  (I : TagList)
  (O : Xml_sigs.Output) : 
  Xml_sigs.Printer with type out := O.out and type xml_elt := Xml.elt
```
```ocaml
module Make_typed
  (Xml : Xml_sigs.Iterable)
  (Typed_xml : Xml_sigs.Typed_xml with module Xml := Xml)
  (O : Xml_sigs.Output) : 
  Xml_sigs.Typed_printer
    with type out := O.out
     and type 'a elt := 'a Typed_xml.elt
     and type doc := Typed_xml.doc
```
```ocaml
module Make_simple
  (Xml : Xml_sigs.Iterable)
  (I : TagList) : 
  Xml_sigs.Simple_printer with type xml_elt := Xml.elt
```
```ocaml
module Make_typed_simple
  (Xml : Xml_sigs.Iterable)
  (Typed_xml : Xml_sigs.Typed_xml with module Xml := Xml) : 
  Xml_sigs.Typed_simple_printer
    with type 'a elt := 'a Typed_xml.elt
     and type doc := Typed_xml.doc
```