
include Makefile.config
-include Makefile.local

all: META
	$(MAKE) -C syntax byte opt
	$(MAKE) -C lib byte opt

byte: META
	$(MAKE) -C syntax byte
	$(MAKE) -C lib byte

opt: META
	$(MAKE) -C syntax opt
	$(MAKE) -C lib opt

clean:
	$(MAKE) -C lib clean
	$(MAKE) -C syntax clean
	$(MAKE) -C doc clean
	rm -f META

distclean:
	$(MAKE) -C syntax distclean
	$(MAKE) -C lib distclean
	$(MAKE) -C doc distclean
	-rm -f *~ \#* .\#* META

.PHONY: doc
doc:
	$(MAKE) -C doc

depend:
	$(MAKE) -C syntax depend
	$(MAKE) -C lib depend

include Makefile.filelist
VERSION := $(shell head -n 1 VERSION)

META: META.in Makefile.config
	sed -e s%_LIBNAME_%${LIBNAME}%g \
	    -e s%_PACKAGENAME_%${PACKAGENAME}%g \
	    $< > $@

install:
	$(OCAMLFIND) install ${PACKAGENAME} \
	  -patch-version ${VERSION} \
	  META ${INTF} ${IMPL} ${NATIMPL} \
	  ${MLI_TO_INSTALL}

install-byte:
	$(OCAMLFIND) install ${PACKAGENAME} \
	  -patch-version ${VERSION} \
	  META ${INTF} ${IMPL} ${MLI_TO_INSTALL}

install-opt:
	$(OCAMLFIND) install ${PACKAGENAME} \
	  -patch-version ${VERSION} \
	  META ${INTF} ${NATIMPL} ${MLI_TO_INSTALL}

uninstall:
	$(OCAMLFIND) remove ${PACKAGENAME}

reinstall: uninstall install
reinstall-byte: uninstall install-byte
reinstall-opt: uninstall install-opt

dist:
	DARCS_REPO=$(PWD) darcs dist -d ${PACKAGENAME}-${VERSION}
