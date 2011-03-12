include Makefile.config

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

distclean:
	$(MAKE) -C xhtml distclean
	$(MAKE) -C syntax distclean
	$(MAKE) -C xml-pretty distclean
	-rm -f *~ \#* .\#*

depend:
	$(MAKE) -C xhtml depend
	$(MAKE) -C syntax depend
	$(MAKE) -C xml-pretty depend

