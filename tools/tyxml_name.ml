
(* Elements and attributes are technically utf8, but ascii is enough for now.

   see <http://www.w3.org/TR/html51/syntax.html#syntax>
*)

(* In the ocaml parser:
   let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
*)
let is_identchar = function
  | 'A'..'Z'
  | 'a'..'z'
  | '_'
  | '\''
  | '0'..'9' -> true
  | _ -> false

let to_ocaml s =
  let f c = if is_identchar c then c else '_' in
  String.map f s


let mapi f s =
  let l = String.length s in
  String.init l (fun i -> f i (String.get s i))


let ident s =
  let s = to_ocaml s in
  mapi (fun i c ->
    if i = 0 then Char.lowercase c else c)
    s

let attrib s =
  "a_" ^ to_ocaml s

let polyvar s =
  let s = to_ocaml s in
  let s = mapi (fun i c ->
    if i = 0 then Char.uppercase c else c)
    s in
  "`" ^ s
