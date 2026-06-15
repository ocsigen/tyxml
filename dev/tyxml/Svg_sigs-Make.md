
# Module `Svg_sigs.Make`

Signature functor for [`Svg_f.Make`](./Svg_f-Make.md).


## Parameters

```ocaml
module Xml : Xml_sigs.T
```

## Signature

```ocaml
module type T =
  T
    with type 'a Xml.W.t = 'a Xml.W.t
     and type 'a Xml.W.tlist = 'a Xml.W.tlist
     and type ('a, 'b) Xml.W.ft = ('a, 'b) Xml.W.ft
     and type Xml.uri = Xml.uri
     and type Xml.event_handler = Xml.event_handler
     and type Xml.mouse_event_handler = Xml.mouse_event_handler
     and type Xml.keyboard_event_handler = Xml.keyboard_event_handler
     and type Xml.touch_event_handler = Xml.touch_event_handler
     and type Xml.attrib = Xml.attrib
     and type Xml.elt = Xml.elt
```
See [`Svg_sigs.T`](./Svg_sigs-module-type-T.md).
