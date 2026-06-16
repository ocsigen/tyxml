
# Module `Tyxml_syntax.Svg_sigs_reflected`

```ocaml
val attribute_parsers : 
  (string * (Common.lang -> Attribute_value.vparser)) list
```
```ocaml
val renamed_attributes : (string * string * string list) list
```
```ocaml
val labeled_attributes : 'a list
```
```ocaml
val element_assemblers : (string * Element_content.assembler) list
```
```ocaml
val renamed_elements : 'a list
```