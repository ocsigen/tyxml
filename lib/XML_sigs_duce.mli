(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2011 Pierre Chambart, Grégoire Henry
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

module type Printer = sig

  val print_list:
    output:(string -> unit) ->
      ?encode:(string -> string) ->
        Ocamlduce.Load.anyxml list -> unit

end

module type TypedXML = sig

    module Info : XML_sigs.Info
    type elt
    type doc

    val of_doc : doc -> Ocamlduce.Load.anyxml
    val of_elt : elt -> Ocamlduce.Load.anyxml

end

module type RawTypedPrinter = sig
  val print_list:
    output:(string -> unit) ->
    ?encode:(string -> string) ->
    {{ Ocamlduce.Load.anyxml }} list -> unit

  val print:
    output:(string -> unit) ->
    ?encode:(string -> string) ->
    ?advert:string-> {{ Ocamlduce.Load.anyxml }} -> unit
end

module type TypedPrinter = sig

  module TypedXML : TypedXML

  type elt = TypedXML.elt
  type doc = TypedXML.doc
  val print_list:
    output:(string -> unit) ->
    ?encode:(string -> string) ->
    elt list -> unit
  val print:
    output:(string -> unit) ->
    ?encode:(string -> string) ->
    ?advert:string-> doc -> unit

end
