
# TyXML

Tyxml is a library for building statically correct HTML and SVG documents.

```ocaml
open Tyxml
let to_ocaml = Html.(a ~a:[a_href "ocaml.org"] [txt "OCaml!"])
```

## Using TyXML


### Standalone Use

To use TyXML in standalone manner, simply install the `tyxml` OPAM package, link the `tyxml` ocamlfind library and open [`Tyxml`](./Tyxml.md).


### Use with another library

TyXML combinators can be used in conjunction with other libraries. Please consult the relevant document. For example, `Eliom` and `Js_of_ocaml`.


### Use with the PPX

TyXML can also be used with the standard HTML syntax, using [the PPX syntax extension](./ppx.md):

```ocaml
open Tyxml
let%html to_ocaml = "<a href='ocaml.org'>OCaml!</a>"
```
This syntax is available both while using TyXML standalone, or with another library. In order to do so, install the `tyxml-ppx` OPAM package and link the `tyxml.ppx` `ocamlfind` library.


## Examples and documentation

For standalone use, examples are available in the [examples](https://github.com/ocsigen/tyxml/tree/master/examples) directory. The entry point of the documentation is in the [`Tyxml`](./Tyxml.md) module.


## Creating documents with TyXML

This section assumes you have at your disposal an `Html` or `Svg` module, as instructed in *the previous section*.

The documentation for TyXML combinators is provided in [`Html_sigs.T`](./Html_sigs-module-type-T.md) and [`Svg_sigs.T`](./Svg_sigs-module-type-T.md) and is common to all instances of `Html` and `Svg`.

The first thing to understand about TyXML is that for most intents and purposes, it is exactly like HTML. As such, the [HTML reference](https://developer.mozilla.org/en-US/docs/Web/HTML/Element) is still very useful. For each HTML element or attribute, there is a combinator implementing it. The main differences are that you can use OCaml to manipulate the elements and that invalid markup produces a type error.

In this tutorial, we will build the [Mini website](https://github.com/ocsigen/tyxml/tree/master/examples/mini_website). If you prefer the native HTML syntax, you can also use the [PPX syntax extension](./ppx.md) and consult the [PPX mini website](https://github.com/ocsigen/tyxml/tree/master/examples/mini_website_ppx).

Let us start by building the content of our website. For text, we use the `txt` combinator. In traditional Web fashion, we put everything in a `div`.

```ocaml
let mycontent =
  div [
    txt "This is a fabulous content." ;
  ]
```
The variable `mycontent` is of type `[> `Div] Html.elt`. As we can see, the fact that this is a `div` is reflected in the type. HTML elements are of type [elt](./Html_sigs-module-type-T.md#type-elt) and have a combinator of the same name, except when it's a reserved OCaml keyword (such as `object_`).

Our content is fabulous, but for the sake of CSS styling (and still in true Web fashion) we want to add a `class` to it.

```ocaml
let mycontent =
  div ~a:[a_class ["content"]] [
    txt "This is a fabulous content." ;
  ]
```
The [a\_class](./Html_sigs-module-type-T.md#val-a_class) creates a new `class` attribute of type `[> `Class] attrib`. Similarly to elements, the kind of attribute is reflected in the [attrib](./Html_sigs-module-type-T.md#type-attrib) type. We use the optional argument `~a` to pass the list of attributes. This optional argument is available on all element combinators.

In order to add a title to our fabulous content, we use the [h1](./Html_sigs-module-type-T.md#val-a) combinator.

```ocaml
let mycontent =
  div ~a:[a_class ["content"]] [
    h1 [txt "A fabulous title"] ;
    txt "This is a fabulous content." ;
  ]
```
Naturally, `div` accepts several children. In TyXML vocabulary, this is a [star](./Html_sigs-module-type-T.md#type-star) combinator. There are also [unary](./Html_sigs-module-type-T.md#type-unary) and [nullary](./Html_sigs-module-type-T.md#type-nullary) combinators, which accept, respectively, one child and zero children.

[title](./Html_sigs-module-type-T.md#val-title) is an example of a `unary` combinator.

```ocaml
let mytitle = title (txt "A Fabulous Web Page")
```

#### Interlude about type errors

However, what would happen if we were to try to put **bold** text in our title? This is not specification-compliant\! Let's try it.

```ocaml
let mytitle = title (b [txt "A Bold Web Page"])
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type ([> Html_types.b ] as 'a) elt
       but an expression was expected of type
         ([< Html_types.title_content_fun ] as 'b) elt = 'b elt
       Type 'a = [> `B ] is not compatible with type 'b = [< `PCDATA ]
       The second variant type does not allow tag(s) `B
```
As expected, this code does not type-check\! The type checker is unfortunately a bit unclear about the source of the error.

It tells us that the given expression has type `[> b] elt` (indeed, it is produced by a `b` combinator) but an expression is expected of type `[< title_content_fun] elt` (which means that is is used as content for a `title` element). It then tells us that, since `[> b] = [> `B]` and `[< title_content_fun] = [< `PCDATA ]`, ``B` is not allowed inside a `title`.

In order to get reasonable type errors with TyXML, The `-short-paths` option should always be used when invoking OCaml.


#### Finishing up the webpage

To finish our webpage, we use [body](./Html_sigs-module-type-T.md#val-body), [head](./Html_sigs-module-type-T.md#val-head) and [html](./Html_sigs-module-type-T.md#val-html). The last two combinators have special types due to their specific constraints: [head](./Html_sigs-module-type-T.md#val-head) requires only one `title` child, and [html](./Html_sigs-module-type-T.md#val-html) requires exactly two children: `head` and `body`.

```ocaml
let mypage =
  html
    (head mytitle [])
    (body [mycontent])
```
If you are using `Eliom` or `Js_of_ocaml`, this is the end of TyXML's territory. However, for standalone use, we now need to print our document as an HTML file. The standalone implementation comes with a printer, [`Tyxml.Html.pp`](./Tyxml_html.md#val-pp), that we can use to print files:

```ocaml
let () =
  let file = open_out "index.html" in
  let fmt = Format.formatter_of_out_channel file in
  pp () fmt mypage;
  close_out file
```
You could also print directly to a string:

```ocaml
let s = Format.asprintf "%a" (Html.pp ()) mypage
```
Well done, you know have a very minimal (but fabulous) website\! Once again, the implementation can be found [here](https://github.com/ocsigen/tyxml/tree/master/examples/mini_website).

Other examples are available in the [examples](https://github.com/ocsigen/tyxml/tree/master/examples/) directory.


## Using your own underlying implementation

You can use TyXML with any underlying implementation. In order to do so, TyXML provides a set of functors. Please consult [the relevant manual](./functors.md).
