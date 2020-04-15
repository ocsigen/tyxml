open Html_types

(* We test various type coercion that should always hold *)

let x = Obj.magic 0 (* get a strong 'a *)

(* Part of the phrasing hierarchy *)
let _ =
  (x : phrasing_without_interactive :> phrasing)
let _ =
  (x : phrasing_without_dfn :> phrasing)

(* Full flow hierarchy *)
let _ =
  (x : flow5_without_form :> flow5)
let _ =
  (x : flow5_without_sectioning_heading_header_footer_address
   :> flow5_without_sectioning_heading_header_footer)
let _ =
  (x : flow5_without_sectioning_heading_header_footer
   :> flow5_without_header_footer)
let _ =
  (x : flow5_without_header_footer
   :> flow5)

(* Part of the flow derived hierarchies *)
let _ =
  (x : flow5_without_interactive_header_footer
   :> flow5_without_interactive)
let _ =
  (x : flow5_without_media_sectioning_heading_header_footer
   :> flow5_without_media)
