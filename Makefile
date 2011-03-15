include Makefile.config

all:
	$(MAKE) -C xhtml byte opt
	$(MAKE) -C syntax byte opt
	$(MAKE) -C xml-pretty byte opt

byte:
	$(MAKE) -C xhtml byte
	$(MAKE) -C syntax byte
	$(MAKE) -C xml-pretty byte

opt:
	$(MAKE) -C xhtml opt
	$(MAKE) -C syntax opt
	$(MAKE) -C xml-pretty opt

clean:
	$(MAKE) -C xhtml clean
	$(MAKE) -C syntax clean
	$(MAKE) -C xml-pretty clean
	$(MAKE) -C doc clean

distclean:
	$(MAKE) -C xhtml distclean
	$(MAKE) -C syntax distclean
	$(MAKE) -C xml-pretty distclean
	$(MAKE) -C doc distclean
	-rm -f *~ \#* .\#*

.PHONY: doc
doc:
	$(MAKE) -C doc

depend:
	$(MAKE) -C xhtml depend
	$(MAKE) -C syntax depend
	$(MAKE) -C xml-pretty depend

include Makefile.filelist
VERSION := $(shell head -n 1 VERSION)

install:
	$(OCAMLFIND) install xmlp4 \
	  -patch-version ${VERSION} \
	  META ${INTF} ${IMPL} ${NATIMPL}

install-byte:
	$(OCAMLFIND) install xmlp4 \
	  -patch-version ${VERSION} \
	  META ${INTF} ${IMPL}

install-opt:
	$(OCAMLFIND) install xmlp4 \
	  -patch-version ${VERSION} \
	  META ${INTF} ${NATIMPL}

uninstall:
	$(OCAMLFIND) remove xmlp4

reinstall: uninstall install
reinstall-byte: uninstall install-byte
reinstall-opt: uninstall install-opt