let file = {|
let _xml_pp = Xml.pp () ;;
#install_printer _xml_pp ;;

let _svg_pp = Svg.pp () ;;
#install_printer _svg_pp ;;
let _svg_pp_elt fmt x = Svg.pp_elt () fmt x ;;
#install_printer _svg_pp_elt ;;

let _html5_pp = Html5.pp () ;;
#install_printer _html5_pp ;;
let _html5_pp_elt fmt x = Html5.pp_elt () fmt x ;;
#install_printer _html5_pp_elt ;;
|}

let eval_string
      ?(print_outcome = false) ?(err_formatter = Format.err_formatter) str =
  let lexbuf = Lexing.from_string str in
  let phrases = !Toploop.parse_use_file lexbuf in
  let f phrase =
    ignore (Toploop.execute_phrase print_outcome err_formatter phrase)
  in
  List.iter f phrases

let () = ignore (eval_string file)
