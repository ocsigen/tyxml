
(** Syntax extension for HTML5, XHTML or SVG tree creation. *)

(**

To choose actual XML-implementation you have to provide a module named [Html5]
(or [Xhtml] or [Svg] respectively):

For example, the following code:
{[
  let module Html5 = Html5.M in
  << <html>
     <head><title></title></head>
     <body><h1>plop</h1></body>
   </html> >>
)
]}
is a caml value of type {v Html5_types.html Html5.elt v}.

In the following, [Html5] is assumed to be an HTML5 implementation, such as
[Html5.M], but this could for example also be Eliom's [Eliom_content.Html5.F].

To compile a module containing this syntax, you need the camlp4 preprocessor:
{[
ocamlfind ocamlc -package tyxml.syntax -syntax camlp4o -c your_module.ml
]}
or
{[
ocamlc -pp "camlp4o -I <path/to/tyxml> pa_tyxml.cmo" -c your_module.ml
]}

You can insert OCaml expressions of type {v 'a Html5.M.elt v} inside html using {v $...$ v}, like this:
{[
let oc = << <em>Ocsigen</em> >> in
<< <p>$oc$ will revolutionize web programming.</p> >>
]}
You can insert OCaml expressions of type string inside html using {v $str:... $ v}, like this:
{[
let i = 4 in
<< <p>i is equal to $str:string_of_int i$</p> >>
]}
If you want to use a dollar in your page, just write it twice.

You can write a list of html5 expressions using the syntax {v <:html5list<...>> v}, for example:
{[
<:html5list< <p>hello</p> <div></div> >>
]}

Here are some other examples showing what you can do:
{[
<< <ul class=$ulclass$ $list:other_attrs$>
     $first_il$
     $list:items$
   </ul> >>
]}

Warning: lists antiquotations are allowed only at the end (before a closing tag). For example, the following is not valid:
{[
<< <ul $list:other_attrs$ class=$ulclass$>
     $list:items$
     $last_il$
   </ul> >>
]}

The syntax extension is not allowed in patterns for the while.

Additionnaly, you may use [ xhtml ], [ xhtmllist ], [ svg ] or [ svglist ].

*)
