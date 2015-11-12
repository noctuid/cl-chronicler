LISP ?= sbcl
PREFIX ?= /usr/local
BINDIR = $(PREFIX)/bin
FILES=chronicler.asd $(shell grep -o ":file \".*\"" chronicler.asd | \
	awk 'gsub("\"","") {print $$2".lisp"}')

chronicler: $(FILES) chronicler.ros
	ros install quicklisp # is this necessary?
	ros -L $(LISP) build chronicler.ros

clean:
	rm *.fasl
	rm chronicler

install:
	install -D -m 755 chronicler "$(DESTDIR)$(BINDIR)"

unintall:
	rm "$(DESTDIR)$(BINDIR)"/chronicler
