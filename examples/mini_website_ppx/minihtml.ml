open Tyxml

let mycontent = [%html5 {|
  <div class="content">
    <h1>A fabulous title</h1>
    This is a fabulous content.
  </div>
|}]


let mytitle = Html5.pcdata "A Fabulous Web Page"

let mypage = [%html5
  "<html><head><title>"mytitle"</title></head><body>"mycontent"</body></html>"]

let () =
  let file = open_out "index.html" in
  let fmt = Format.formatter_of_out_channel file in
  Html5.pp () fmt mypage;
  close_out file
