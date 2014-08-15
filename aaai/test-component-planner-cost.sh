#!/bin/bash

TIMER="/usr/bin/time -f 'real %e\nuser %U\nsys %S\nmaxmem %M'"

run(){
    echo $1
    log=${1%.*}.component-planner-cost.log
    err=${1%.*}.component-planner-cost.err
    rm -f $log $err
    ulimit -v 3000000 -t 1900
    /usr/bin/time -f 'real %e\nuser %U\nsys %S\nmaxmem %M' \
        ./component-planner-cost -v $1 > $log 2> $err
    if [[ $(cat ${1%.*}.plan) != "" ]]
    then
        echo plan found!
    fi
}

# # run elevators-sat11/p01.pddl
# run cell-assembly-noneg-nocost/p01.pddl

for problem in $(find -name "p01.pddl")
do
    run $problem
done

for problem in $(find -name "p04.pddl")
do
    run $problem
done
