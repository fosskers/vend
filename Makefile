LISP ?= sbcl
LOAD_FLAG ?= --load

.PHONY: install clean

vend: build.lisp vend.asd src/*.lisp
	$(LISP) $(LOAD_FLAG) build.lisp

install: vend
	mkdir -p ~/.local/bin/
	cp vend ~/.local/bin/

clean:
	-rm vend
