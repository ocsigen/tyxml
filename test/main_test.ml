let () = Alcotest.run "tyxml" (
  Test_html.tests
  @ Test_ppx.tests
)
