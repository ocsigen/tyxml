open Tyxml

(* Basic alcotest machinery *)

let to_string = Format.asprintf "%a" (Html.pp_elt ())

let tyxml_tests l =
  let f (name, (ty : Html_types.body_content Html.elt), s) =
    name, `Quick, fun () -> Alcotest.(check string) name (to_string ty) s
  in
  List.map f l


(* Boilerplate to make writing the PPX and JSX tests easier *)

module type LANGUAGE = sig
  type 'a data
  type 'a children
  include Xml_sigs.Typed_pp with type 'a elt := 'a data
  val pp_children : Format.formatter -> 'a children -> unit
  val totl : Xml.children -> 'a children
  val toeltl : 'a children -> Xml.children
end

module TyTests (Language : LANGUAGE) = struct
  module Testable = struct
    type t = Xml.children
    let pp fmt x =
      Language.pp_children
        fmt (Language.totl x)
    let equal = (=)
  end

  let make l =
    let f (name, ty1, ty2) =
      name, `Quick, fun () ->
        Alcotest.(check (module Testable)) name
          (Language.toeltl ty1) (Language.toeltl ty2)
    in
    List.map f l
end

module Html = struct
  include Tyxml.Html
  let pp_children fmt =
    Format.pp_print_list ~pp_sep:(fun _ () -> ()) (pp_elt ()) fmt
end
module Svg = struct
  include Tyxml.Svg
  let pp_children fmt =
    Format.pp_print_list ~pp_sep:(fun _ () -> ()) (pp_elt ()) fmt
end
module HtmlTests = TyTests (Html)
module SvgTests = TyTests (Svg)


(* The regular HTML module, but with most type equality hidden.
   This forces the use of the wrapping functions
*)
module HtmlWrapped : sig
  include Html_sigs.T
    with type Xml.data = Tyxml.Xml.data
     and type 'a data = 'a Html.data
  include LANGUAGE
    with type 'a data := 'a data
     and type 'a children = 'a Html.data Xml.Child.list
     and type doc := doc
end = struct
  include Html
  module Svg = Svg
end
module HtmlWrappedTests = TyTests(HtmlWrapped)

let (@:) h t =  HtmlWrapped.Xml.(Child.cons (Elt.inject h) t)
let (@-) =  HtmlWrapped.Xml.Child.append
let nil = HtmlWrapped.Xml.Child.nil
let (!) = HtmlWrapped.Xml.Child.return
let (!:) x = x @: nil ()

let (!!) = HtmlWrapped.Xml.Attr.return
