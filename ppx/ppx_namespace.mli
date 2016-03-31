(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2016 Anton Bachin
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

(** Namespace-specific values. *)



val reflect :
  Location.t -> string -> Ppx_common.lang * (module Ppx_sigs_reflected.S)
(** When given either [Markup.Ns.html] or [Markup.Ns.svg] as argument, evaluates
    to the title of the corresponding markup language, the name of the run-time
    module containing its TyXML implementation, and a preprocessing-time module
    containing reflection information. *)

val get : Ppx_common.lang -> (module Ppx_sigs_reflected.S)
(** Similar to {!reflect} but takes a {!Ppx_common.lang} directly. *)

val to_lang : Location.t -> string -> Ppx_common.lang
(** Takes a namespace and returns the appropriate language. *)
