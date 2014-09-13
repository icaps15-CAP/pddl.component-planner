#!/bin/bash

i=0

for f in $(ls -Sr1 prob*.pddl)
do
    i=$(( $i + 1 ))
    if [[ $i -lt 10 ]]
    then
        mv $f p0$i.pddl
    else
        mv $f p$i.pddl
    fi        
done

