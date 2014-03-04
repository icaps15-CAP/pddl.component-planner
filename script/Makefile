


repl:
	sbcl --core ~/repos/core.original --script script.lisp repl

executable: repl
	./repl-image --script script.lisp image

clean:
	rm repl-image lispimage