(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2013 Gabriel Radanne <drupyog+caml@zoho.com>
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
 * Foundation, Inc., 51 Franklin Street, Suite 500, Boston, MA 02110-1301, USA.
*)

module type APP = sig
  type 'a t
  val return : 'a -> 'a t

  type (-'a, 'b) ft
  val fmap : ('a, 'b) ft -> 'a t -> 'b t
end

module type MANY = sig
  type 'a t
  val return : 'a -> 'a t

  type 'a tlist
  val nil : unit -> 'a tlist
  val singleton : 'a t -> 'a tlist
  val cons : 'a t -> 'a tlist -> 'a tlist
  val append : 'a tlist -> 'a tlist -> 'a tlist
end

module type T = sig
  type +'a t
  include APP with type 'a t := 'a t
  include MANY with type 'a t := 'a t
end

module type NoWrap =
  T with type 'a t = 'a
     and type 'a tlist = 'a list
     and type (-'a, 'b) ft = 'a -> 'b

module NoWrap = struct
  type 'a t = 'a
  type 'a tlist = 'a list
  type (-'a, 'b) ft = 'a -> 'b
  external return : 'a -> 'a = "%identity"
  let fmap f :  'a t -> 'b t = f

  let nil () = []
  let singleton x = [x]
  let cons x xs = x::xs
  let append x y= x@y
end
