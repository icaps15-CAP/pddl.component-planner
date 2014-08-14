#!/bin/bash

for dir in $(ls -d)
do
    echo $dir
    ./component-planner $dir/p01.pddl 2>&1 | tee $dir/p01.component-planner.log
done
