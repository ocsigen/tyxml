open Html5.M

let this_title = title (pcdata "Your Cool Web Page")

let image_box =
  div ~a:[a_id "image_box"]
    []

let links_box =
  ul ~a:[a_class ["links_bar"]; a_id "links_bar"]
    [li ~a:[a_id "home_click"]
       [pcdata "My Musings"];
     li ~a:[a_id "about_click"]
       [pcdata "About Me"];
     li ~a:[a_id "blog_posts_click"]
       [pcdata "Blog"];
     li ~a:[a_id "hackathons_click"]
       [pcdata "Hackathons"]]

let common_footer =
  footer ~a:[a_id "footer_box"]
    [p [pcdata "This site was made with ";
        a ~a:[a_href "http://ocaml.org"] [pcdata "OCaml"];
        pcdata " and ";
        a ~a:[a_href "https://www.gnu.org/software/emacs/"] [pcdata "emacs"]]]

let home_content =
  div
    [h2
       [pcdata "Hello Coder"]]

let main_payload =
  div ~a:[a_id "payload"]
    [home_content]

let common_nav =
  nav [links_box]

let content_box =
  div ~a:[a_id "content_box"]
    [common_nav;
     main_payload;
     common_footer]

let main_script =
  script ~a:[a_src (Xml.uri_of_string "main.js")] (pcdata "")

let home_page_doc =
  html (head this_title
          [link ~rel:[`Stylesheet] ~href:"home.css" ();])
    (body [image_box; content_box; main_script])

let pages = [("index.html", home_page_doc)]

let () =
  List.iter begin fun (page_name, a_page) ->
    Printf.sprintf "Generating: %s" page_name |> print_endline;
    let file_handle = open_out page_name in
    Html5.P.print (output_string file_handle) a_page;
    close_out file_handle;
    match Sys.command (Printf.sprintf "tidy -im -ashtml %s" page_name) with
    | 0 -> ()
    | c ->
      print_endline "You don't have tidy, no pretty printing of html"
  end
    pages
