include Makefile.variables

MLI=color.mli color__HSI.mli color__SRGB24.mli
MLSRC=color.ml color__HSI.ml color__SRGB24.ml
MLINIT=
CSRC=

LIBNAME=color
STUBLIBNAME=

OCAMLCFLAGS=-w +a
CFLAGS=

LDFLAGS=
LDLIB=
CCLIB=

include Makefile.rules

$(BUILDDIR)/color.cmo $(BUILDDIR)/color.cmx $(BUILDDIR)/color.cmi: \
	private override OCAMLCFLAGS+=-no-alias-deps -w -49
$(BUILDDIR)/color__HSI.cmi $(BUILDDIR)/color__SRGB24.cmi: $(BUILDDIR)/color.cmi
$(BUILDDIR)/color__HSI.cmx $(BUILDDIR)/color__SRGB24.cmx: $(BUILDDIR)/color.cmx
