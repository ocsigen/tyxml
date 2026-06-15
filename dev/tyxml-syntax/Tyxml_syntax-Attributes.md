
# Module `Tyxml_syntax.Attributes`

Attribute parsing.

```ocaml
val parse : 
  Ppxlib.Location.t ->
  Common.name ->
  (Common.name * string Common.value) list ->
  (Ppxlib.arg_label * Ppxlib.expression) list
```
`parse loc element_name attributes` evaluates to a list of labeled parse trees, each representing an attribute argument to the element function for `element_name`. For example, if called on the HTML element `<img src='foo' alt='bar' id='some-image'>`, this function will evaluate to parse trees for the arguments:

```ocaml
~src:(return "foo") ~alt:(return "bar") ~a:[id (return "some-image")]
```
This satisfies the attribute arguments in the signature of `Html_sigs.T.img`.
