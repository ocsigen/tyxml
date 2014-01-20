include Makefile.config
-include Makefile.local

all: files/META files/META.tyxml
	$(MAKE) -C syntax byte opt
	$(MAKE) -C lib byte opt

byte: files/META files/META.tyxml
	$(MAKE) -C syntax byte
	$(MAKE) -C lib byte

opt: files/META files/META.tyxml
	$(MAKE) -C syntax opt
	$(MAKE) -C lib opt

clean:
	$(MAKE) -C lib clean
	$(MAKE) -C syntax clean
	$(MAKE) -C doc clean
	rm -f files/META files/META.tyxml

distclean:
	$(MAKE) -C syntax distclean
	$(MAKE) -C lib distclean
	$(MAKE) -C doc distclean
	-rm -f *~ \#* .\#* files/META files/META.tyxml

.PHONY: doc
doc:
	$(MAKE) -C doc

depend:
	$(MAKE) -C syntax depend
	$(MAKE) -C lib depend

include Makefile.filelist
VERSION := $(shell head -n 1 VERSION)

files/META: files/META.in Makefile.config
	sed -e s%_LIBNAME_%${LIBNAME}%g \
	    -e s%_PACKAGENAME_%${PACKAGENAME}%g \
            -e s%_LIBDIR_%% \
            -e s%_SYNTAXDIR_%% \
	   $< > $@

files/META.tyxml: files/META.in Makefile.config
	sed -e s%_LIBNAME_%${LIBNAME}%g \
	    -e s%_PACKAGENAME_%${PACKAGENAME}%g \
            -e s%_LIBDIR_%directory\ =\ \"..\/lib\"% \
            -e s%_SYNTAXDIR_%directory\ =\ \"..\/syntax\"% \
	  $< > $@

install:
	$(OCAMLFIND) install ${PACKAGENAME} \
	  -patch-version ${VERSION} \
	  files/META ${INTF} ${IMPL} ${NATIMPL} \
	  ${MLI_TO_INSTALL}

install-byte:
	$(OCAMLFIND) install ${PACKAGENAME} \
	  -patch-version ${VERSION} \
	  files/META ${INTF} ${IMPL} ${MLI_TO_INSTALL}

install-opt:
	$(OCAMLFIND) install ${PACKAGENAME} \
	  -patch-version ${VERSION} \
	  files/META ${INTF} ${NATIMPL} ${MLI_TO_INSTALL}

uninstall:
	$(OCAMLFIND) remove ${PACKAGENAME}

reinstall: uninstall install
reinstall-byte: uninstall install-byte
reinstall-opt: uninstall install-opt

dist:
	DARCS_REPO=$(PWD) darcs dist -d ${PACKAGENAME}-${VERSION}
