(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

exception Input_is_too_large
exception Ocsigen_Request_interrupted of exn
exception Ocsigen_Bad_Request
exception Ocsigen_Request_too_long

let id x = x

let comp f g x = f (g x)

let rec list_remove a = function
    [] -> []
  | b::l when a = b -> l
  | b::l -> b::(list_remove a l)

let rec list_assoc_remove a = function
    [] -> raise Not_found
  | (b,c)::l when a = b -> c,l
  | b::l -> let v,ll = list_assoc_remove a l in v,b::ll


(* virtual hosts: *)
type virtual_host_part = Text of string * int | Wildcard
type virtual_hosts = ((virtual_host_part list) * int option) list

(* static pages *)
type static_dir = Static_dir of string option * (string * static_dir) list



let remove_cookie_str = "; expires=Wednesday, 09-Nov-99 23:12:40 GMT"

(** various functions for URLs *)
let defaultpagename = "index"
    
let remove_slash = function
    [] -> []
  | ""::l -> l
  | l -> l
    
let change_empty_list = function
    [] -> [""] (* It is not possible to register an empty URL *)
  | l -> l


(* This function is in Neturl (split_path)
   let rec cut_url s = 
   try
   let length = String.length s in
   if length = 0 then []
   else
   let pos_slash = String.index s '/' in
   if pos_slash = 0 
   then cut_url (String.sub s 1 (length-1))
   else 
   let prefix = String.sub s 0 pos_slash in
   (*  if length > (pos_slash+1)
      then *)
   prefix::(cut_url (String.sub s (pos_slash+1) (length - pos_slash - 1)))
   (* else [prefix] *)
   with _ -> [s]
 *)


let rec string_of_url_path = function
    [] -> ""
  | [a] -> a
  | a::l -> a^"/"^(string_of_url_path l)

let rec string_of_url_path_suff u = function
    None -> string_of_url_path u
  | Some suff -> let deb = (string_of_url_path u) in
    if deb = "" then suff else deb^"/"^suff

let reconstruct_absolute_url_path current_url = string_of_url_path_suff

let reconstruct_relative_url_path current_url u suff =
  let rec drop cururl desturl = match cururl, desturl with
  | a::l, [b] -> l, desturl
  | [a], m -> [], m
  | a::l, b::m when a = b -> drop l m
  | a::l, m -> l, m
  | [], m -> [], m
  in let rec makedotdot = function
    | [] -> ""
(*    | [a] -> "" *)
    | _::l -> "../"^(makedotdot l)
  in 
  let aremonter, aaller = drop current_url u
  in let s = (makedotdot aremonter)^(string_of_url_path_suff aaller suff) in
(*  Messages.debug ((string_of_url_path current_url)^"->"^(string_of_url_path u)^"="^s);*)
  if s = "" then defaultpagename else s

let rec relative_url_path_to_myself = function
    []
  | [""] -> defaultpagename
  | [a] -> a
  | a::l -> relative_url_path_to_myself l
