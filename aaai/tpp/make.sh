#!/bin/bash

pnum=0

list="10 15 20"
for m in $list
do
    for p in $list
    do
        for t in $list
        do
            for d in $list
            do
                for l in $list
                do
                    pnum=$(($pnum+1))
                    if [[ $pnum -ge 10 ]]
                    then
                        pnum_zero=$pnum
                    else
                        pnum_zero=0$pnum
                    fi
                    ./gen-TPP -s 2014 -m $m -p $p -t $t -d $d -l $l p$pnum_zero.pddl 
                done
            done
        done
    done
done
