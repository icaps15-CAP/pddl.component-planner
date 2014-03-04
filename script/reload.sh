#! /bin/bash


./repl-image <<EOF
(pddl.component-planner-test::reload)
(pddl.component-planner-test::reload-save-repl)
EOF

./repl-image <<EOF
(pddl.component-planner-test::reload-save-image)
EOF
