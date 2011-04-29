(* Ocsigen
 * Copyright (C) 2011 Jaap Boender
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

(** Printer for XHTML with Ocamlduce that handles browser specificities properly. *)

module Make (I : sig val emptytags : string list end) : XML_sigs_duce.Printer

(* module MakeTypedRaw (TypedXML : XML_sigs_duce.TypedXML) : XML_sigs_duce.RawTypedPrinter *)
module MakeTyped (TypedXML : XML_sigs_duce.TypedXML) :
  XML_sigs_duce.TypedPrinter with module TypedXML := TypedXML

val print:
  output:(string -> unit) ->
    ?encode:(string -> string) ->
      Ocamlduce.Load.anyxml -> unit
