BUILD:=$(shell $(CC) -dumpmachine)
HOST=$(BUILD)
TARGET=$(HOST)

PROGRAMPREFIX=$(addsuffix -,$(filter-out $(BUILD),$(TARGET)))

OCAMLC=$(PROGRAMPREFIX)ocamlc
OCAMLOPT=$(or $(OCAMLC:c=opt),$(PROGRAMPREFIX)ocamlopt)
RLWRAP?=

OCAMLCFLAGS=
OCAMLOPTFLAGS=$(OCAMLCFLAGS)
LDFLAGS=

BUILDSUFFIX=.noindex
BUILDDIR=$(TARGET)$(BUILDSUFFIX)
COLOR_BUILDDIR=$(BUILDDIR)/color$(BUILDSUFFIX)

BINLN=$(and $(filter $(BUILD),$(TARGET)), \
        $(if $(findstring mingw,$(BUILD))$(findstring msys,$(BUILD)),,bin))

EXAMPLES=$(basename $(wildcard *.ml))
TESTS=$(filter test_%,$(EXAMPLES))

.PHONY: all check interacitve clean $(TESTS)

all: \
	$(and $(OCAMLC),$(patsubst %,$(BUILDDIR)/%.byte.exe,$(EXAMPLES))) \
	$(and $(OCAMLOPT),$(patsubst %,$(BUILDDIR)/%.opt.exe,$(EXAMPLES))) \
	$(BINLN)

$(BUILDDIR)/%.byte.exe: %.ml $(BUILDDIR)/color.cma
	$(OCAMLC) -c -o $(BUILDDIR)/$*.cmo -I $(BUILDDIR) $(OCAMLCFLAGS) $<
	$(OCAMLC) -o $@ -I $(BUILDDIR) color.cma $(BUILDDIR)/$*.cmo

$(BUILDDIR)/%.opt.exe: %.ml $(BUILDDIR)/color.cmxa
	$(OCAMLOPT) -c -o $(BUILDDIR)/$*.cmx -I $(BUILDDIR) $(OCAMLOPTFLAGS) $<
	$(OCAMLOPT) $(strip \
		-o $@ -I $(BUILDDIR) color.cmxa $(BUILDDIR)/$*.cmx \
		$(addprefix -ccopt ,$(LDFLAGS)))

$(and $(OCAMLC),$(BUILDDIR)/color.cma) \
$(and $(OCAMLOPT),$(BUILDDIR)/color.cmxa)&: \
		../source/color*
	$(MAKE) $(strip \
		-C ../source install \
		BUILDDIR=$(abspath $(COLOR_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR)))

$(BINLN):
	ln -s $(BUILDDIR) $@

check: all $(TESTS)

$(TESTS): %: \
		$(and $(OCAMLC),$(BUILDDIR)/%.byte.exe) \
		$(and $(OCAMLOPT),$(BUILDDIR)/%.opt.exe)
	$(and $(OCAMLC),$<)
	$(and $(OCAMLOPT),$(BUILDDIR)/$*.opt.exe)

interactive: $(BUILDDIR)/color.cma
	$(RLWRAP) ocaml -I $(BUILDDIR) $(OCAMLCFLAGS) color.cma

clean:
	-$(and $(BINLN),[ -h "$(BINLN)" ] && rm "$(BINLN)")
	-$(MAKE) $(strip \
		-C ../source clean uninstall \
		BUILDDIR=$(abspath $(COLOR_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR)))
	-rm $(BUILDDIR)/*.exe $(BUILDDIR)/*.cm? $(BUILDDIR)/*.o
	-rmdir $(BUILDDIR)
