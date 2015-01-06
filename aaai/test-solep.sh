#!/bin/bash

TIMER="/usr/bin/time -f 'real %e\nuser %U\nsys %S\nmaxmem %M'"

run(){
    echo $1
    log=${1%.*}.component-planner.log
    err=${1%.*}.component-planner.err
    rm -f $log $err
    ulimit -v 3000000 -t 600
    time ./component-planner \
        --dynamic-space-size 2000 \
        --plain --remove-main-problem-cost \
        --training $(dirname $1)/t01.pddl \
        --training $(dirname $1)/t02.pddl \
        --training $(dirname $1)/t03.pddl \
        --both-search solep-clean ' ' -t 3600 -m 2000000 -v \
        $1 2> $err | tee $log
    if [[ $(cat ${1%.*}.plan) != "" ]]
    then
        echo plan found!
    fi
}

# # run elevators-sat11/p01.pddl
# run cell-assembly-noneg-nocost/p01.pddl

./component-planner

finalize(){

    kill $pid
    exit 1
}
trap "finalize" SIGHUP SIGINT SIGQUIT
pid=

problems="elevators-ipc11/p10.pddl tpp/p20.pddl gripper/p20.pddl"

# transport-sat11-strips/p10.pddl

for problem in $problems
do
    run $problem &
    pid=$!
    wait $pid
done

# for problem in $(find -name "p04.pddl")
# do
#     run $problem
# done


