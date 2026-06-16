
# Module type `Sigs_reflected.S`

```ocaml
val attribute_parsers : 
  (string * (Common.lang -> Attribute_value.vparser)) list
```
Pairs `tyxml_attribute_name, wrapped_attribute_value_parser`.

```ocaml
val renamed_attributes : (string * string * string list) list
```
Triples `tyxml_attribute_name, markup_name, in_element_types`.

```ocaml
val labeled_attributes : 
  (string * string * (Common.lang -> Attribute_value.vparser)) list
```
Triples `tyxml_element_name, label, wrapped_attribute_value_parser`.

```ocaml
val element_assemblers : (string * Element_content.assembler) list
```
Pairs `tyxml_element_name, child_argument_assembler`.

```ocaml
val renamed_elements : (string * string) list
```
Pairs `markup_element_name, tyxml_name`.
