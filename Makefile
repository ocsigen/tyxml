.PHONY: default
default: build

.PHONY: build
build: 
	jbuilder build --dev @install

.PHONY: tools
tools: 
	jbuilder build --dev tools/autoname.exe
	@echo "You can now use: 'jbuilder exec tools/autoname.exe -- element'"

.PHONY: test
test:
	jbuilder runtest --dev

.PHONY: clean
clean:
	jbuilder clean

.PHONY: doc
doc:
	jbuilder build @doc

.PHONY: doc-api-wiki
wikidoc: build
	make -C doc wikidoc
