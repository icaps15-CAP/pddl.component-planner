#!/bin/bash


for f in */*.typed-pddl
do
    mkdir -p ../typed/$(dirname $f)
    mv $f ../typed/$(dirname $f)/$(basename $f .typed-pddl).pddl
done
