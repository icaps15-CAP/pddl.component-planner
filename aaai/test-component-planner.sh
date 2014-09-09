#!/bin/bash

TIMER="/usr/bin/time -f 'real %e\nuser %U\nsys %S\nmaxmem %M'"

run(){
    echo $1
    log=${1%.*}.component-planner.log
    err=${1%.*}.component-planner.err
    rm -f $log $err
    ulimit -v 3000000 -t 1900
    time ./component-planner --dynamic-space-size 2000 \
        --preprocess-ff \
        --use-grounded-actions \
        --disable-filtering \
        --validation -v \
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
for problem in $(find -name "p01.pddl")
do
    run $problem &
    pid=$!
    wait $pid
done

# for problem in $(find -name "p04.pddl")
# do
#     run $problem
# done


