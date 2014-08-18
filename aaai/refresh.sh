#!/bin/bash

sbcl --quit \
    --eval "(ql:register-local-projects)" \
    --eval '(push :interpret-pddl *features*)' \
    --eval '(ql:quickload :pddl.component-planner.experiment)'


