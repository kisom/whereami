LIBSRC :=	$(shell ls src/Location/*.hs)
MAINSRC :=	app/Main.hs
INSTDIR :=	$(shell stack path --local-install-root)
TARGET :=		$(INSTDIR)/bin/whereami-exe

$(TARGET): $(LIBSRC) $(MAINSRC)
	stack build

.PHONY: format
format:
	brittany --write-mode inplace $(LIBSRC) $(MAINSRC)