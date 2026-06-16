
# The functorial interface

TyXML provides a functorial interface to implement HTML and SVG on top of any XML implementation. This is used heavily by Eliom to implement the `F` and `D` modules, providing respectively a functional and a DOM version of the HTML implementation.


## The `Make` functors

These interfaces are available in the modules [`Html_f`](./Html_f.md) and [`Svg_f`](./Svg_f.md). They provide a functor `Make` which takes a concrete implementation of Xml following [`Xml_sigs.T`](./Xml_sigs-module-type-T.md). A default implementation is provided by the [`Tyxml.Xml`](./Tyxml_xml.md) module. The functor [`Html_f.Make`](./Html_f-Make.md) also needs an Svg implementation that one can obtain, for example, with the functor [`Svg_f.Make`](./Svg_f-Make.md). The `Xml` always needs to provide a module `W` of type [`Xml_wrap.T`](./Xml_wrap-module-type-T.md), along with types

```ocaml
 type 'a wrap = 'a W.t 
```
and

```ocaml
 type 'a list_wrap = 'a W.tlist 
```
. The purpose of the `Wrap` module is explained in the next section.


## Wrapping up the nodes

The module [`Xml_sigs.T.W`](./Xml_sigs-module-type-T-W.md) allows to wrap Xml elements in a monad `'a t`. A good example of application is the `R` modules with reactive nodes in Eliom. Here is the simplified signature of the `div` element:

```ocaml
R.div : 'a elt list t -> div elt
```
`t` will wrap the input of every `Xml` node and be integrated in the resulting node.

The `W` module needs to implement operations over the type of nodes, and provides a special type for lists of nodes. The `W` module additionally provides a type `(-'a, 'b) ft` for (wrapped) functions, whose purpose is explained in the next section. In most cases, it is sufficient to define:

```ocaml
 (-'a, 'b) ft = 'a -> 'b
```
An identity wrapper, [`Xml_wrap.NoWrap`](./Xml_wrap-module-type-NoWrap.md), is provided. [`Xml_wrap.NoWrap`](./Xml_wrap-module-type-NoWrap.md) can be used to apply the functor without wrapping the elements.


## The `Make_with_wrapped_functions` functors

The `Make_with_wrapped_functions` functors (available in the modules [`Html_f`](./Html_f.md) and [`Svg_f`](./Svg_f.md)) differ from the `Make` functors by requiring an additional argument `C` (of type [`Html_f.Wrapped_functions`](./Html_f-Wrapped_functions.md) and [`Svg_f.Wrapped_functions`](./Svg_f-Wrapped_functions.md) respectively).

The `C` functor argument defines a type `(-'a, 'b) ft`, and a collection of `ft` values. For applying the functor, the type constraint

```ocaml
 ('a, 'b) Xml.W.ft = ('a, 'b) C.ft
```
needs to be satisfied. The `ft` values are wrapped functions that the functor uses internally to operate on wrapped elements.

The motivation of providing the `Make_with_wrapped_functions` is as follows. Certain monads `'a t` can only be operated upon by wrapped functions, and not by plain OCaml functions. The wrapped functions cannot be produced internally by TyXML, and thus have to be provided to the functor. Our intended application is with Eliom shared (i.e., client-server) signals. Such signals can only be operated upon with Eliom shared functions.

The `Make` functors simply apply the `Make_with_wrapped_functions` functors with TyXML-provided `Wrapped_functions` modules. The latter modules do not wrap the functions, i.e., they satisfy:

```ocaml
(-'a, 'b) ft = 'a -> 'b
```
.


## Exporting the correct signature

In order to help export the correct signature after a functor application, two signature functors are provided: [`Svg_sigs.Make`](./Svg_sigs-Make.md) and [`Html_sigs.Make`](./Html_sigs-Make.md).

As an example of use, let us look at the module [`Tyxml.Svg`](./Tyxml_svg.md). Here is the definition of the module:

```ocaml
module M = Svg_f.Make(Tyxml_xml)
```
In this case, the declaration in the interface file should look like this: 

```ocaml
module M : Svg_sigs.Make(Tyxml_xml).T
```
The signature functor [`Svg_sigs.Make`](./Svg_sigs-Make.md) contains only a signature `T`, which is equal to [`Svg_sigs.T`](./Svg_sigs-module-type-T.md), but exports various equalities with the module `Xml`.

You should **always** use a signature functor to give the type of a module produced by a functor application. It will ensure that exactly the right type equalities are exported and will naturally keep track of changes in TyXML.

There are some important notes about these signature functors:

- `module M : Svg_sigs.Make(Tyxml_xml).T` doesn't mean that `M.Xml` is a submodule of `Xml`. It only means that the types `uri`, `event_handler`, `mouse_event_handler`, `keyboard_event_handler`, `touch_event_handler`, `attrib` and `elt` are the same in both modules. This is useful when not exporting the exact module that was used in the functor, but another (smaller and simpler) module. This is the case in `Js_of_ocaml_tyxml.Tyxml_js.R`, for example.
- [`Html_f`](./Html_f.md) and [`Svg_f`](./Svg_f.md) functors export two additional equalities, `+'a elt = Xml.elt` and `+'a attrib = Xml.attrib`. These equalities **should never be exported in a public interface**. Exporting them would break HTML typing by allowing to build invalid HTML trees. These equalities are useful internally, for example in Eliom they are used to make `F.elt`, `D.elt` and `R.elt` equals.