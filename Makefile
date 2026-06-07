.PHONY: default
default: build

.PHONY: build
build:
	dune build @install

.PHONY: tools
tools:
	dune build tools/autoname.exe
	@echo "You can now use: 'dune exec tools/autoname.exe -- element'"

.PHONY: test
test:
	dune runtest

.PHONY: clean
clean:
	dune clean

.PHONY: doc
doc:
	dune build @doc
# The themed documentation site is built by wodoc from doc/wodoc; see
# doc/README.md. (The legacy ocamldoc/wikidoc targets have been removed.)

