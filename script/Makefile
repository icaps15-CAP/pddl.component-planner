
.PHONY: repl exec

all: exec

repl: repl-image
exec: lispimage

repl-image: script.lisp
	sbcl --core ~/repos/core.original --load script.lisp repl

lispimage: repl-image
	./repl-image --load script.lisp image

clean:
	rm repl-image lispimage