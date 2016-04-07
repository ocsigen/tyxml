
(** Syntax extension for HTML or SVG tree creation. *)

(**

To choose actual XML-implementation you have to provide a module named [Html]
(or [Svg]):

For example, the following code:
{[
  let open Tyxml in
  << <html>
     <head><title></title></head>
     <body><h1>plop</h1></body>
   </html> >>
)
]}
is a caml value of type {v Html_types.html Html.elt v}.

In the following, [Html] is assumed to be an HTML implementation, such as
{!Tyxml.Html}, but this could for example also be Eliom's [Eliom_content.Html.F].

To compile a module containing this syntax, you need the camlp4 preprocessor:
{[
ocamlfind ocamlc -package tyxml.syntax -syntax camlp4o -c your_module.ml
]}
or
{[
ocamlc -pp "camlp4o -I <path/to/tyxml> pa_tyxml.cmo" -c your_module.ml
]}

You can insert OCaml expressions of type {v 'a Html.elt v} inside html using {v $...$ v}, like this:
{[
let oc = << <em>Ocsigen</em> >> in
<< <p>$oc$ will revolutionize web programming.</p> >>
]}
You can insert OCaml expressions of type string inside HTML using {v $str:... $ v}, like this:
{[
let i = 4 in
<< <p>i is equal to $str:string_of_int i$</p> >>
]}
If you want to use a dollar in your page, just write it twice.

You can write a list of HTML expressions using the syntax {v <:htmllist<...>> v}, for example:
{[
<:htmllist< <p>hello</p> <div></div> >>
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

Additionnaly, you may use [ svg ] or [ svglist ].

*)
