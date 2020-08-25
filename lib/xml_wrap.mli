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

module type NODE = sig
  type 'a t
  type 'a child
  val inject : 'a t -> 'a child
end

module type APP = sig
  type 'a t
  val return : 'a -> 'a t

  type (-'a, 'b) ft
  val fmap : ('a, 'b) ft -> 'a t -> 'b t
end

module type MANY = sig
  type 'a t
  val return : 'a -> 'a t

  type 'a list
  val nil : unit -> 'a list
  val singleton : 'a t -> 'a list
  val cons : 'a t -> 'a list -> 'a list
  val append : 'a list -> 'a list -> 'a list
end

module type NoWrap = sig
  type 'a t = 'a
  type 'a child = 'a
  val return : 'a -> 'a
  val inject : 'a -> 'a

  type (-'a, 'b) ft = 'a -> 'b
  val fmap : ('a, 'b) ft -> 'a t -> 'b t

  type 'a list = 'a List.t
  val nil : unit -> 'a list
  val singleton : 'a -> 'a list
  val cons : 'a -> 'a list -> 'a list
  val append : 'a list -> 'a list -> 'a list
end

module NoWrap : NoWrap
