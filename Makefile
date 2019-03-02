## The type of library we want to build. Possible values:
##   relocatable
##   static
WIKI_LIBRARY_TYPE=static

# You may edit this makefile as long as you keep these original 
# target names defined.
MODE=@MODE@
GNATMAKE=@GNATMAKE@
GNATCLEAN=gnatclean
GPRINSTALL=@GPRINSTALL@
INSTALL=@INSTALL@
DYNAMO=dynamo

GPRPATH=wikiada.gpr

BUILDS_SHARED=@BUILDS_SHARED@

HAVE_ADA_UTIL=@WIKI_USE_ADA_UTIL@

version=@WIKI_VERSION@

distdir=ada-wiki-@WIKI_VERSION@

DIST_FILE=ada-wiki-@WIKI_VERSION@.tar.gz

LN_S=@LN_S@
MKDIR=mkdir
CP=cp
LN=ln -s

ifeq (${OS},Windows_NT)
LIBEXT=dll
LIBVER=dll
else
LIBEXT=so
LIBVER=so.$(version)
endif

srcdir = .
top_srcdir = @top_srcdir@
VPATH = @srcdir@
prefix = @prefix@
exec_prefix = @exec_prefix@
top_builddir = .

# share/ada/adainclude
infix_inc=@ADA_INC_BASE@

# share/ada/adainclude
infix_prj=@ADA_PRJ_BASE@

# lib
infix_lib=@ADA_LIB_BASE@

# lib/ada/adalib
infix_ali=@ADA_ALI_BASE@

projectdir=${prefix}/${infix_prj}
bindir=${prefix}/bin

libname=libada_wiki
docdir=libada-wiki
samplesdir=${prefix}/share/doc/$(docdir)

PROCESSORS=@NR_CPUS@
COVERAGE=@BUILDS_COVERAGE@
MAKE_ARGS=-XMODE=${MODE} -XCOVERAGE=${COVERAGE}
MAKE_ARGS += -XWIKI_LIBRARY_TYPE=${WIKI_LIBRARY_TYPE} -XPROCESSORS=$(PROCESSORS)
ifeq ($(WIKI_LIBRARY_TYPE),relocatable)
MAKE_ARGS += -XUTIL_BUILD=relocatable -XUTIL_LIBRARY_TYPE=relocatable
MAKE_ARGS += -XXMLADA_BUILD=relocatable
MAKE_ARGS += -XLIBRARY_TYPE=relocatable
endif

ifeq (${BUILDS_SHARED},yes)
all:     static shared
install: uninstall install_shared install_static install-info
else
all:     static
install: uninstall install_static install-info
endif

# Build executables for all mains defined by the project.
build:	setup
	$(GNATMAKE) -m -p -P"$(GPRPATH)" $(MAKE_ARGS)
ifeq ($(HAVE_ADA_UTIL),yes)
	$(GNATMAKE) -p -Pwikiada_tests $(MAKE_ARGS)
endif

static:
	$(MAKE) WIKI_LIBRARY_TYPE=static build

shared:
	$(MAKE) WIKI_LIBRARY_TYPE=relocatable build

setup: obj/wiki/static lib/static

obj/wiki/static lib/static:
	$(MKDIR) -p $@

# Not intended for manual invocation.
# Invoked if automatic builds are enabled.
# Analyzes only on those sources that have changed.
# Does not build executables.
autobuild:
	$(GNATMAKE) $(MAKE_ARGS) -gnatc -c -k  -P "$(GPRPATH)"

# Clean the root project of all build products.
clean:	clean_test
	-rm -rf lib obj bin

# Clean the files produced by the unit tests
clean_test:
	rm -rf regtests/result/*

# Clean root project and all imported projects too.
clean_tree:
	$(GNATCLEAN) -q -P "$(GPRPATH)" -r

# Check *all* sources for errors, even those not changed.
# Does not build executables.
analyze:
	$(GNATMAKE) $(MAKE_ARGS) -f  -gnatc -c -k  -P "$(GPRPATH)"

# Clean, then build executables for all mains defined by the project.
rebuild: clean build

# Build and run the unit tests
test:	build
ifeq ($(HAVE_ADA_UTIL),yes)
	bin/wiki_harness -xml wiki-aunit.xml
else
	@echo "WARNING: Tests are not compiled and not executed because Ada Utility Library is not used"
endif

doc:
	$(DYNAMO) build-doc -markdown wiki

dist:
	git archive -o $(DIST_FILE) --prefix=$(distdir)/ HEAD

install_static:
	$(MAKE) WIKI_LIBRARY_TYPE=static install_lib

install_shared:
	$(MAKE) WIKI_LIBRARY_TYPE=relocatable install_lib

install_lib:
	$(GPRINSTALL) -p -f --prefix=${prefix} $(MAKE_ARGS) \
		--build-name=$(WIKI_LIBRARY_TYPE) $(GPRPATH)

install-info:
	@echo "Environment setup:"
	@echo "  export ADA_PROJECT_PATH=${projectdir}:$$ADA_PROJECT_PATH"

install_samples:
	$(MKDIR) -p $(samplesdir)/samples
	cp -rp $(srcdir)/samples/*.ad[sb] $(samplesdir)/samples/
	cp -p $(srcdir)/samples.gpr $(samplesdir)
	cp -p $(srcdir)/config.gpr $(samplesdir)

uninstall:
	-$(GPRINSTALL) -q -f --prefix=${prefix} $(MAKE_ARGS) --uninstall $(GPRPATH)
