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

let get : Common.lang -> (module Sigs_reflected.S) = function
  | Html -> (module Html_sigs_reflected)
  | Svg  -> (module Svg_sigs_reflected)

let to_lang loc ns =
  if ns = Markup.Ns.html then Common.Html
  else if ns = Markup.Ns.svg then Common.Svg
  else Common.error loc "Unknown namespace %s" ns

let reflect loc ns =
  let l = to_lang loc ns in (l, get l)
