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

let parse loc ((ns, name) as element_name) attributes children =
  let attributes = Ppx_attributes.parse loc element_name attributes in

  let language, implementation, (module Reflected) =
    Ppx_namespace.reflect loc ns in

  let name =
    try List.assoc name Reflected.renamed_elements
    with Not_found -> name
  in

  let element_function =
    Ppx_common.qualify implementation name
    |> Ppx_common.identifier loc
  in

  let assembler =
    try List.assoc name Reflected.element_assemblers
    with Not_found -> Ppx_common.error loc "Unknown %s element %s" language name
  in

  let children = assembler implementation loc name children in

  Ast_helper.Exp.apply ~loc element_function (attributes @ children)
