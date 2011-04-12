(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2007 Gabriel Kerneis
 * Copyright (C) 2009 Boris Yakobowski
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

(* This is the complementary parser to Xmllexer, used in mode Xhtml with
   inline Ocaml code. The rules below are used to parse subexpressions
   starting with '$' *)

{ open Xmllexer }

let camlidentchar =  [^ '$' ]
let newline = ['\n']

rule camlident c  = parse
  | newline {
      update_loc c 1 false 0 ;
      store c ;
      camlident c lexbuf
    }
  | camlidentchar+ {
      store c ;
      camlident c lexbuf
    }
  | '$' { buff_contents c }

and token_dollar c = parse
  |"str:" {
      `CamlString (camlident c lexbuf)
    }
  |"list:"  {
      `CamlList (camlident c lexbuf)
    }
  | "$" { `PCData "$" }
  | ""  {
      `CamlExpr (camlident c lexbuf)
    }

and attribute_dollar c loc = parse
  | "" {
      ignore (buff_contents c);
      `CamlAttributes (camlident c lexbuf)
    }

and attr_name_dollar c loc = parse
  | ""  {
      ignore (buff_contents c) ;
      `CamlAttrName (camlident c lexbuf)
    }

and attr_data_dollar c loc = parse
  | "" {
      `CamlAttrVal (camlident c lexbuf)
    }
