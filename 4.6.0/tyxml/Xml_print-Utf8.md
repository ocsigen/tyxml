
# Module `Xml_print.Utf8`

Utf8 normalizer and encoder for HTML.

Given a `pp` function produced by one of the functors in [`Xml_print`](./Xml_print.md), this modules is used as following:

```ocaml
  let encode x = fst (Xml_print.Utf8.normalize_html x) in
  Format.printf "%a" (Html.pp ~encode ()) document
```
```ocaml
type utf8 = string
```
```ocaml
val normalize : string -> utf8 * bool
```
`normalize str` take a possibly invalid utf-8 string and return a valid utf-8 string where invalid bytes have been replaced by the replacement character `U+FFFD`. The returned boolean is true if invalid bytes were found

```ocaml
val normalize_html : string -> utf8 * bool
```
Same as `normalize` plus some extra work : It encode '\<' , '\>' , '"' , '&' characters with corresponding entities and replaced invalid html character by `U+FFFD`
