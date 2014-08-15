#!/bin/bash



sbcl --dynamic-space-size 2500 \
    --disable-debugger \
    --load main.lisp 

sbcl --dynamic-space-size 2500 \
    --disable-debugger \
    --eval '(push :add-cost *features*)' \
    --load main.lisp
