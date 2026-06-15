
# TyXML Ppx syntax extension

TyXML provides a ppx to allow writing HTML and SVG documents in the standard syntax while still enjoying the benefits and type-safety of TyXML. It is available as the `tyxml-ppx` package:

```ocaml
opam install tyxml-ppx
```

## Syntax

The ppx provides two quotations, `html` and `svg` (and two aliases, `tyxml.html` and `tyxml.svg`).

```ocaml
open Tyxml ;;

let content = [%html{|<div id="content">some content</div>|}] ;;
val content : [> Html_types.div ] Html.elt

let svgpath = [%svg{|<path d="M 0 1 L 1 0"></path>|}] ;;
val svgpath : [> Svg_types.path ] Svg.elt
```
The quotations will use the `Html` and `Svg` module that are available in scope. To use another module name, you can qualify the quotation:

```ocaml
let content = [%html.F {|<div id="content">some content</div>|}]
val content : [> Html_types.div ] F.elt
```
A quotation containing multiple elements will automatically produce a list of elements:

```ocaml
let my_text = [%html
  {|This is an <b>HTML</b> formated <i>content</i>.|}]
val my_text : [> `B | `I | `PCDATA ] Html.elt list
```
The produced elements can be used inside usual TyXML combinators:

```ocaml
let my_span = Html.(span ~a:[a_class ["mytext"]] my_text)
val my_span : [> Html_types.span ] Html.elt
```
It is also possible to use tyxml elements inside quotations using `antiquotations`:

```ocaml
let my_paragraphs =
  [%html "<p>"[my_span]"</p><p>more content</p>"]
val my_paragraphs : [> Html_types.p ] Html.elt list
```
Note here that since `p` expects a list of children (it's a [star](./Html_sigs-module-type-T.md#type-star) element), the antiquotation must be of type list, hence the use of `[], []`.

It is also possible to use antiquotations for attributes.

```ocaml
let my_id = "thediv"
let my_div = [%html "<div id="my_id"></div>"]
val my_div : [> Html_types.div ] Html.elt
```

## "Unsafe" attributes

Some Javascript libraries and frameworks depend upon HTML markup that includes non-standard attributes, which tyxml would usually reject. When constructing elements using tyxml's API directly, the available workaround is to use the [Unsafe](./Html_sigs-module-type-T-Unsafe.md) constructors. The same relaxed semantics can be had when using the ppx by prefixing non-standard attribute names with a leading underscore.

So, while this will fail:

```ocaml
[%html {|<button hx_post="/clicked" hx_swap="outerHTML">Click Me</button>|}]
```
This will not:

```ocaml
[%html {|<button _hx_post="/clicked" _hx_swap="outerHTML">Click Me</button>|}]
```
Such underscore-prefixed attributes are presumed to be strings, and are constructed using e.g. [Unsafe.string\_attrib](./Html_sigs-module-type-T-Unsafe.md#val-string_attrib).


## Let notation

It is also possible to use the ppx with the `let` notation:

```ocaml
let%html content = {|<div id="content">some content</div>|} ;;
val content : [> Html_types.div ] Html.elt
```
All the capabilities provided by the ppx are still available with this form. Additionally, the modifiers `and` or `rec` are available. It is also possible to create functions:

```ocaml
let%html make_content id = "<div id="id" >some content</div>" ;;
val make_content : string -> [> Html_types.div ] Html.elt
```

## Notes


### Locations

Due to the code transformations done by the ppx, proper locations are difficult to provide. Please report examples of badly located code on [the bug tracker](https://github.com/ocsigen/tyxml/issues).


### Composability

Due to various reasons, some `HTML` can not be composed properly using the ppx. For example, this will result in an error:

```ocaml
let my_title = [%html "<title>The title</title>"]
let my_head = [%html "<head>"my_title"</head>"]
```
You can, however, inline the title element inside the head element:

```ocaml
let my_title = [%html "The title"]
let my_head = [%html "<head><title>"my_title"</title></head>"]
```