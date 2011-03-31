
include Makefile.config

all:
	$(MAKE) -C lib byte opt
	$(MAKE) -C syntax byte opt

byte:
	$(MAKE) -C lib byte
	$(MAKE) -C syntax byte

opt:
	$(MAKE) -C lib opt
	$(MAKE) -C syntax opt

clean:
	$(MAKE) -C lib clean
	$(MAKE) -C syntax clean
	$(MAKE) -C doc clean

distclean:
	$(MAKE) -C lib distclean
	$(MAKE) -C syntax distclean
	$(MAKE) -C doc distclean
	-rm -f *~ \#* .\#*

.PHONY: doc
doc:
	$(MAKE) -C doc

depend:
	$(MAKE) -C lib depend
	$(MAKE) -C syntax depend

include Makefile.filelist
VERSION := $(shell head -n 1 VERSION)

install:
	$(OCAMLFIND) install ${PACKAGENAME} \
	  -patch-version ${VERSION} \
	  META ${INTF} ${IMPL} ${NATIMPL}

install-byte:
	$(OCAMLFIND) install ${PACKAGENAME} \
	  -patch-version ${VERSION} \
	  META ${INTF} ${IMPL}

install-opt:
	$(OCAMLFIND) install ${PACKAGENAME} \
	  -patch-version ${VERSION} \
	  META ${INTF} ${NATIMPL}

uninstall:
	$(OCAMLFIND) remove ${PACKAGENAME}

reinstall: uninstall install
reinstall-byte: uninstall install-byte
reinstall-opt: uninstall install-opt
