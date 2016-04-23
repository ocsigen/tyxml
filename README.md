# TyXML

TyXML is an HTML and SVG templating library. It uses OCaml's type system to
validate markup at compile time:

```ocaml
open Tyxml

let%html hello = "<li>Hello</li>"
let%html world = "<li>World</li>"
let%html image = "<img src='/like.png' alt='Like this'>"

let%html greet = "<ul>" [hello; world; image] "</ul>"
                                    (* ^^^^^
> Error: This expression has type (... snip ...)
         The second variant type does not allow tag(s) `Img *)
```

since `<ul>` elements should not contain `<img>` elements directly. If you
remove `image` from the list in `greet`, the example compiles, and you can
print:

```ocaml
Format.printf "%a" (Html.pp_elt ()) greet     (*
> <ul><li>Hello</li><li>World!</li></ul> *)
```

TyXML checks element nesting, presence of correct attributes, attribute values,
and some other properties. See the [documentation][manual] and
[examples](examples).

[manual]: https://ocsigen.org/tyxml/manual/

#### Combinators

The above example uses TyXML's PPX syntax, which translates to TyXML
combinators. If written directly with combinators, the example looks like this:

```ocaml
open Tyxml.Html

let hello = li [pcdata "Hello"]
let world = li [pcdata "World!"]
let greet = ul [hello; world]
```

As you can see, combinators can be cleaner than HTML. They can, however, also be
more complicated:

```ocaml
let field = input ~a:[a_input_type `Password] ()
```

For some purposes, combinators are the only option. Either way, the error
messages and API reference deal with the combinators, so it's important to be
aware of them even when using the PPX.

## Installation

Without the PPX:

```sh
opam install tyxml
```

With the PPX:

```sh
opam install markup tyxml
```

This gives `ocamlfind` packages `tyxml` and, optionally, `tyxml.ppx`.

## Applications

TyXML is used in [Eliom][] and [Js_of_ocaml][].

[Eliom]: https://ocsigen.org/eliom/manual/clientserver-html
[Js_of_ocaml]: https://ocsigen.org/js_of_ocaml/api/Tyxml_js
