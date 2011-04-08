
include Makefile.config

all: META
	$(MAKE) -C lib byte opt
	$(MAKE) -C syntax byte opt

byte: META
	$(MAKE) -C lib byte
	$(MAKE) -C syntax byte

opt: META
	$(MAKE) -C lib opt
	$(MAKE) -C syntax opt

clean:
	$(MAKE) -C lib clean
	$(MAKE) -C syntax clean
	$(MAKE) -C doc clean
	rm -f META

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

META: META.in Makefile.config
	sed -e s%_LIBNAME_%${LIBNAME}%g \
	    -e s%_PACKAGENAME_%${PACKAGENAME}%g \
	    $< > $@

install:
	$(OCAMLFIND) install ${PACKAGENAME} \
	  -patch-version ${VERSION} \
	  META ${INTF} ${IMPL} ${NATIMPL} \

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
