#!/bin/bash



sbcl --dynamic-space-size 8000 \
    --disable-debugger \
    --eval '(push :interpret-pddl *features*)' \
    --load main.lisp 

sbcl --dynamic-space-size 8000 \
    --disable-debugger \
    --eval '(push :interpret-pddl *features*)' \
    --eval '(push :add-cost *features*)' \
    --load main.lisp
