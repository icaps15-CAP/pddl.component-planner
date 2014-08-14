#!/bin/bash

TIMER="/usr/bin/time -f 'real %e\nuser %U\nsys %S\nmaxmem %M'"

run(){
    echo $1
    log=${1%.*}.component-planner.log
    rm -f $log
    ulimit -v 3000000 -t 1900
    /usr/bin/time -f 'real %e\nuser %U\nsys %S\nmaxmem %M' \
        ./component-planner -v $1 &> $log
    if [[ $(cat ${1%.*}.plan) != "" ]]
    then
        echo plan found!
    fi
}

run elevators-sat11/p01.pddl

# for problem in $(find -name "p01.pddl")
# do
#     run $problem
# done

# for problem in $(find -name "p04.pddl")
# do
#     run $problem
# done
