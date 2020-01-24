(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2019 Gabriel Radanne
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Suite 500, Boston, MA 02111-1307, USA.
*)

module Var : sig
  type t
  type gen
  val mk : gen -> t
end = struct
  type t = int
  type gen = int ref
  let mk r = incr r; !r
end

module Make
    (X : Xml_sigs.T with type ('a, 'b) W.ft = ('a -> 'b))
    (S : Svg_sigs.T with module Xml := X)
    (H : Html_sigs.T with module Xml := X and module Svg := S)
  : sig

  type 'a var (* It might be important to *not* make this generalizable *)
  val mk : Var.gen -> 'a var

  module Xml : Xml_sigs.T

  module Svg : sig
    include Svg_sigs.Make(Xml).T

    val var : 'a var -> 'a elt
    val resolve : ('x var -> 'x S.elt) -> 'a elt -> 'a S.elt
  end

  module Html : sig
    include Html_sigs.Make(Xml)(Svg).T

    val var : 'a var -> 'a elt
    val resolve : ('x var -> 'x H.elt) -> 'a elt -> 'a H.elt
  end

end = struct
  
  type 'a var = Var.t
  let mk = Var.mk

  module Xml = struct

    include X
    (** Element *)

    type elt =
      | Raw of X.elt
      | Node of X.ename * X.attrib list * elt X.list_wrap
      | Var of Var.t

    let lift f x = Raw (f x)

    let empty = lift X.empty
    let comment = lift X.comment
    let pcdata = lift X.pcdata
    let encodedpcdata = lift X.encodedpcdata
    let entity = lift X.entity

    let cdata = lift X.cdata
    let cdata_script = lift X.cdata_script
    let cdata_style = lift X.cdata_style

    let leaf ?a name = Raw (X.leaf ?a name)
    let node ?(a=[]) name children = Node (name, a, children)

  end

  let rec resolve f xml = match xml with
    | Xml.Raw x -> x
    | Xml.Node (name, a, children) ->
      let children = X.W.map (resolve f) children in
      X.node ~a name children
    | Xml.Var v -> f v

  module Svg = struct
    include Svg_f.Make (Xml)
    let var v = tot (Var v)
    let resolve f x = S.tot (resolve (fun v -> S.toelt @@ f v) (toelt x))
  end
  module Html = struct
    include Html_f.Make(Xml)(Svg)
    let var v = tot (Var v)
    let resolve f x = H.tot (resolve (fun v -> H.toelt @@ f v) (toelt x))
  end

end

module Implem = Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)

(* module P = Xml_print.Make_typed_fmt(Xml_raw)(Html) *)

(* include M
 * include P *)
