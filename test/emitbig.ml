(* This is an absurd website to stress the printer.
   It creates fibonacci(22) nested divs.
*)

open Tyxml

let rec unfold n =
  let l =
    if n = 1 then []
    else if n = 2 then []
    else[
      unfold (n-1) ;
      unfold (n-2) ;
    ]
  in
  Html.(div ~a:[a_class ["fibo" ^ string_of_int n]] l)

let emit_page_pp indent page =
  let file_handle = open_out "fibo.html" in
  let fmt = Format.formatter_of_out_channel file_handle in
  Format.fprintf fmt "%a@." (Html.pp ~indent ()) page;
  close_out file_handle

let () =
  let p = Html.(
    html (head (title (pcdata "fibo")) []) (body [unfold 22])
  ) in
  let indent = Array.length Sys.argv > 1 && Sys.argv.(1) = "indent" in
  let time_pp = ref 0. in
  let n = 10 in
  for _ = 1 to n do
    let t = Unix.gettimeofday () in
    emit_page_pp indent p ;
    let tpp = Unix.gettimeofday () -. t in
    time_pp := !time_pp +. tpp ;
  done ;
  Printf.printf
    "Time:  %f\n%!"
    (!time_pp /. float n)
