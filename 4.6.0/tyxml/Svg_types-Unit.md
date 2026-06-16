
# Module `Svg_types.Unit`

SVG defines several units to measure time, length, angles.

```ocaml
type 'a quantity = float * 'a option
```
```ocaml
type angle = [ `Deg | `Grad | `Rad ] quantity
```
```ocaml
type length =
  [ `Em | `Ex | `Px | `In | `Cm | `Mm | `Pt | `Pc | `Percent ] quantity
```
```ocaml
type time = [ `S | `Ms ] quantity
```
```ocaml
type frequency = [ `Hz | `KHz ] quantity
```