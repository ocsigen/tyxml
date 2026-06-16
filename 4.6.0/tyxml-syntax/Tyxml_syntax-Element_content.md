
# Module `Tyxml_syntax.Element_content`

Element child argument assemblers. These are almost parsers, except they only tell how to pass already-parsed children to element functions.

```ocaml
type assembler =
  lang:Common.lang ->
  loc:Ppxlib.Location.t ->
  name:string ->
  Ppxlib.expression Common.value list ->
  (Ppxlib.arg_label * Ppxlib.expression) list
```
Assemblers satisfy: `assembler ~lang ~loc ~name children` evaluates to a list of optionally-labeled parse trees for passing `children` to the the element function for element `name`. For example, for a table element

```ocaml
<table>
  <thead>
    <tr><th>A</th><th>B</th></tr>
  </thead>
  <tbody>
  </tbody>
</table>
```
The assembler `table`, when called with the parsed children, will evaluate to parse trees representing

```ocaml
~thead:(* the thead element *) [(* the tbody element *)]
```
This satisfies the child arguments in the signature of `Html_sigs.T.tablex`. The `~table` label is represented by the string `"table"`, and the unlabeled list argument is paired with the empty string.

The argument `implementation` is the name of the module providing the run-time implementation of the element function that will be applied to the children. It is either `Html` or `Svg`, and is based on the element's namespace. It is used for wrapping child elements, and for scoping child `txt` elements.

The `name` argument is used for error reporting.


### Generic

```ocaml
val nullary : assembler
```
```ocaml
val unary : assembler
```
```ocaml
val star : assembler
```

### Special-cased

```ocaml
val html : assembler
```
```ocaml
val head : assembler
```
```ocaml
val figure : assembler
```
```ocaml
val object_ : assembler
```
```ocaml
val audio_video : assembler
```
```ocaml
val table : assembler
```
```ocaml
val fieldset : assembler
```
```ocaml
val datalist : assembler
```
```ocaml
val details : assembler
```
```ocaml
val menu : assembler
```
```ocaml
val picture : assembler
```
```ocaml
val script : assembler
```
```ocaml
val textarea : assembler
```

## Misc utilities

```ocaml
val filter_surrounding_whitespace : 
  Ppxlib.expression Common.value list ->
  Ppxlib.expression Common.value list
```
Remove txt node containing only whitespace that are at the beginning or the end of the list.

```ocaml
val comp_filter_whitespace : assembler -> assembler
```
Improve an assembler by removing txt nodes containing only whitespace
