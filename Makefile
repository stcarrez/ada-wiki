NAME=wikiada

-include Makefile.conf

STATIC_MAKE_ARGS = $(MAKE_ARGS) -XWIKI_LIBRARY_TYPE=static
SHARED_MAKE_ARGS = $(MAKE_ARGS) -XWIKI_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XUTILADA_BASE_BUILD=relocatable -XUTIL_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XXMLADA_BUILD=relocatable
SHARED_MAKE_ARGS += -XLIBRARY_TYPE=relocatable

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

$(eval $(call ada_library,$(NAME)))
