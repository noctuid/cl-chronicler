LISP ?= sbcl
PREFIX ?= /usr/local
BINDIR = $(PREFIX)/bin
LICENSEDIR = $(PREFIX)/share/licenses
FILES=chronicler.asd $(shell grep -o ":file \".*\"" chronicler.asd | \
	awk 'gsub("\"","") {print $$2".lisp"}')

chronicler: $(FILES) chronicler.ros
	ros install quicklisp # is this necessary?
	ros -L $(LISP) build chronicler.ros

clean:
	rm *.fasl
	rm chronicler

install:
	install -D -m 755 chronicler "$(DESTDIR)$(BINDIR)"/chronicler
	install -D -m 644 LICENSE "$(DESTDIR)$(LICENSEDIR)"/chronicler/LICENSE

unintall:
	rm -r "$(DESTDIR)$(BINDIR)"/chronicler
	rm -rf "$(DESTDIR)$(BINDIR)"/chronicler
