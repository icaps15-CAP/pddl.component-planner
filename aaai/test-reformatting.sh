#!/bin/bash


./component-planner --reformat transport-sat11-strips/domain.pddl
./component-planner --reformat transport-sat11-strips/p01.pddl
./component-planner --remove-main-problem-cost --reformat transport-sat11-strips/domain.pddl
./component-planner --remove-main-problem-cost --reformat transport-sat11-strips/p01.pddl
