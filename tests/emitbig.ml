(* This is an absurd website to stress the printer.
   It creates fibonacci(22) nested divs.
*)

let rec unfold n =
  let l =
    if n = 1 then []
    else if n = 2 then []
    else[
      unfold (n-1) ;
      unfold (n-2) ;
    ]
  in
  Html5.M.(div ~a:[a_class ["fibo" ^ string_of_int n]] l)

module FmtP = Xml_print.Make_typed_fmt(Xml)(Html5.M)

let emit_page_pp page =
  let file_handle = open_out "fibo.html" in
  let fmt = Format.formatter_of_out_channel file_handle in
  FmtP.pp fmt page;
  close_out file_handle

let emit_page_str page =
  let file_handle = open_out "fibo.html" in
  Html5.P.print ~output:(output_string file_handle) page;
  close_out file_handle

let () =
  let p = Html5.M.(
    html (head (title (pcdata "fibo")) []) (body [unfold 22])
  ) in
  let time_std = ref 0. in
  let time_pp = ref 0. in
  let n = 10 in
  for _ = 1 to n do
    let t = Unix.gettimeofday () in
    emit_page_str p ;
    let t' = Unix.gettimeofday () in
    emit_page_pp p ;
    let tpp = Unix.gettimeofday () -. t' in
    time_std := !time_std +. t' -. t ;
    time_pp := !time_pp +. tpp ;
  done ;
  Printf.printf
    "Time std: %f\nTime pp:  %f\n%!"
    (!time_std /. float n)
    (!time_pp /. float n)
