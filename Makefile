.PHONY: default
default: build

.PHONY: build
build: 
	jbuilder build --dev @install

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
doc-api-wiki: build
	make -C doc api/wiki/index.wiki
