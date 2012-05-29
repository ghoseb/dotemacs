VERSION=1.1.1
EMACS=emacs
PREFIX=/usr/local
SYSCONFDIR=/etc
ELS=magit.el magit-svn.el magit-topgit.el magit-stgit.el magit-key-mode.el magit-bisect.el
ELS_CONTRIB=contrib/magit-simple-keys.el contrib/magit-classic-theme.el
ELCS=$(ELS:.el=.elc)
ELCS_CONTRIB=$(ELS_CONTRIB:.el=.elc)
DIST_FILES=$(ELS) Makefile magit.texi magit.info README.md magit.spec.in magit-pkg.el.in 50magit.el
DIST_FILES_CONTRIB=$(ELS_CONTRIB) contrib/magit
ELPA_FILES=$(ELS) magit.info magit-pkg.el

.PHONY=install

BATCH=$(EMACS) -batch -q -no-site-file -eval \
  "(setq load-path (cons (expand-file-name \".\") load-path))"

%.elc: %.el
	$(BATCH) --eval '(byte-compile-file "$<")'

all: core docs contrib

core: $(ELCS) magit.spec magit-pkg.el

docs: magit.info

contrib: $(ELCS_CONTRIB)

magit.spec: magit.spec.in
	sed -e s/@VERSION@/$(VERSION)/ < $< > $@

magit-pkg.el: magit-pkg.el.in
	sed -e s/@VERSION@/$(VERSION)/ < $< > $@

magit.elc: magit.el
magit-key-mode.elc:
magit-svn.elc:
magit-topgit.elc:
magit-stgit.elc:
magit.info:

dist: magit-$(VERSION).tar.gz

magit-$(VERSION).tar.gz: $(DIST_FILES) $(DIST_FILES_CONTRIB)
	mkdir -p magit-$(VERSION)/contrib
	cp --preserve=timestamps $(DIST_FILES) magit-$(VERSION)
	cp --preserve=timestamps $(DIST_FILES_CONTRIB) magit-$(VERSION)/contrib
	tar -cvzf magit-$(VERSION).tar.gz magit-$(VERSION)
	rm -rf magit-$(VERSION)

elpa: magit-$(VERSION).tar

magit-$(VERSION).tar: $(ELPA_FILES)
	mkdir magit-$(VERSION)
	cp --preserve=timestamps $(ELPA_FILES) magit-$(VERSION)
	tar -cvf magit-$(VERSION).tar magit-$(VERSION)
	rm -rf magit-$(VERSION)

install: install_core install_docs

install_core: core
	mkdir -p $(DESTDIR)$(PREFIX)/share/emacs/site-lisp
	install -m 644 $(ELS) $(ELCS) $(DESTDIR)$(PREFIX)/share/emacs/site-lisp
	mkdir -p $(DESTDIR)$(SYSCONFDIR)/emacs/site-start.d
	install -m 644 50magit.el $(DESTDIR)$(SYSCONFDIR)/emacs/site-start.d/50magit.el

install_docs: docs
	mkdir -p $(DESTDIR)$(PREFIX)/share/info
	install -m 644 magit.info $(DESTDIR)$(PREFIX)/share/info
	install-info --info-dir=$(DESTDIR)$(PREFIX)/share/info $(DESTDIR)$(PREFIX)/share/info/magit.info

install_contrib: contrib
	mkdir -p $(DESTDIR)$(PREFIX)/share/emacs/site-lisp
	install -m 644 $(ELS_CONTRIB) $(ELCS_CONTRIB) $(DESTDIR)$(PREFIX)/share/emacs/site-lisp
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	install -m 755 contrib/magit $(DESTDIR)$(PREFIX)/bin

install_all: install install_contrib

clean:
	rm -fr magit-pkg.el magit.spec $(ELCS) $(ELCS_CONTRIB) *.tar.gz magit-$(VERSION)
