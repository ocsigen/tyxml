open Tyxml.Html

let mycontent =
  div ~a:[a_class ["content"]] [
    h1 [pcdata "A fabulous title"] ;
    pcdata "This is a fabulous content." ;
  ]

let mytitle = title (pcdata "A Fabulous Web Page")

let mypage =
  html
    (head mytitle [])
    (body [mycontent])

let () =
  let file = open_out "index.html" in
  let fmt = Format.formatter_of_out_channel file in
  Format.fprintf fmt "%a@." (pp ~indent:true ()) mypage;
  close_out file
