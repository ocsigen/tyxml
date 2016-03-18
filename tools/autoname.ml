
let arg = ref false

let answer_elem ~attrib s =
  Printf.printf
    "%s\t%s\t%s\n%!"
    s
    (if attrib then Tyxml_name.attrib s else Tyxml_name.ident s)
    (Tyxml_name.polyvar s)

let spec = [
  "-a", Arg.String (answer_elem ~attrib:true), "Returns the tyxml names for the given attribute." ;
  "-e", Arg.String (answer_elem ~attrib:false), "Returns the tyxml names for the given element." ;

]

let usage =
  "Accepts HTML attributes and elements and returns their tyxml names.\
   Names without option are treated as elements."

let () = Arg.parse spec (answer_elem ~attrib:false) usage
