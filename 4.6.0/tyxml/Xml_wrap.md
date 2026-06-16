
# Module `Xml_wrap`

```ocaml
module type T = sig ... end
```
```ocaml
module type NoWrap =
  T
    with type 'a t = 'a
     and type 'a tlist = 'a list
     and type (-'a, 'b) ft = 'a -> 'b
```
```ocaml
module NoWrap : NoWrap
```