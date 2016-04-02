export ALCOTEST_SHOW_ERRORS=true

opam pin add --no-action tyxml .
opam install -t --deps-only tyxml
opam install --verbose tyxml
opam remove --verbose tyxml
opam install camlp4 markup ppx_tools
opam install -t --verbose tyxml

do_build_doc () {
  make wikidoc
  cp -Rf doc/manual-wiki/*.wiki ${MANUAL_SRC_DIR}
  cp -Rf _build/tyxml-api.wikidocdir/*.wiki ${API_DIR}
}

do_remove () {
  opam remove --verbose tyxml
}
