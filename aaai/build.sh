#!/bin/bash

pids=

sbcl --dynamic-space-size 1900 \
    --disable-debugger \
    --eval '(push :interpret-pddl *features*)' \
    --load main.lisp \
    --eval '(pddl.component-planner.experiment::save "component-planner")' &
pids="$! $pids"

sbcl --dynamic-space-size 1900 \
    --disable-debugger \
    --eval '(push :interpret-pddl *features*)' \
    --eval '(push :add-cost *features*)' \
    --load main.lisp \
    --eval '(pddl.component-planner.experiment::save "component-planner-cost")' &
pids="$! $pids"

sbcl --dynamic-space-size 8000 \
    --disable-debugger \
    --eval '(push :interpret-pddl *features*)' \
    --load main.lisp  \
    --eval '(pddl.component-planner.experiment::save "component-planner-large")' &
pids="$! $pids"

sbcl --dynamic-space-size 8000 \
    --disable-debugger \
    --eval '(push :interpret-pddl *features*)' \
    --eval '(push :add-cost *features*)' \
    --load main.lisp \
    --eval '(pddl.component-planner.experiment::save "component-planner-cost-large")' &
pids="$! $pids"

wait $pids
