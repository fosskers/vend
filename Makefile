.PHONY: install clean

vend: build.lisp vend.asd src/*.lisp
	ecl --load build.lisp

install: vend
	mkdir -p ~/.local/bin/
	cp vend ~/.local/bin/

clean:
	-rm vend
