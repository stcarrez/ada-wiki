NAME=wikiada

-include Makefile.conf

STATIC_MAKE_ARGS = $(MAKE_ARGS) -XWIKI_LIBRARY_TYPE=static
SHARED_MAKE_ARGS = $(MAKE_ARGS) -XWIKI_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XUTILADA_BASE_BUILD=relocatable -XUTIL_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XXMLADA_BUILD=relocatable
SHARED_MAKE_ARGS += -XLIBRARY_TYPE=relocatable

WIKI_DOC=\
  title.md \
  pagebreak.tex \
  index.md \
  pagebreak.tex \
  Installation.md \
  pagebreak.tex \
  Wiki.md \
  pagebreak.tex

DOC_OPTIONS=-f markdown
DOC_OPTIONS+= --listings --number-sections --toc
HTML_OPTIONS=-f markdown
HTML_OPTIONS+= --listings --number-sections --toc --css pandoc.css

include Makefile.defaults

# Build executables for all mains defined by the project.
build-test::	setup
ifeq ($(HAVE_ADA_UTIL),yes)
	$(GNATMAKE) $(GPRFLAGS) -p -P$(NAME)_tests $(MAKE_ARGS)
endif

# Build and run the unit tests
test:	build-test
ifeq ($(HAVE_ADA_UTIL),yes)
	bin/wiki_harness -l $(NAME): -xml wiki-aunit.xml
else
	@echo "WARNING: Tests are not compiled and not executed because Ada Utility Library is not used"
endif

install-samples:
	$(MKDIR) -p $(samplesdir)/samples
	cp -rp $(srcdir)/samples/*.ad[sb] $(samplesdir)/samples/
	cp -p $(srcdir)/samples.gpr $(samplesdir)
	cp -p $(srcdir)/config.gpr $(samplesdir)

samples:
	$(GNATMAKE) $(GPRFLAGS) -p -Psamples $(MAKE_ARGS)

$(eval $(call ada_library,$(NAME)))
$(eval $(call pandoc_build,wikiada-book,$(WIKI_DOC)))
$(eval $(call alire_publish,alire.toml,wi/wikiada,wikiada-$(VERSION).toml))

genentities:
	$(GNATMAKE) $(GPRFLAGS) -p -Psupport $(MAKE_ARGS)
	cd support && ../bin/genentities > ../src/wiki-html_parser-entities.ads

.PHONY: samples genentities

