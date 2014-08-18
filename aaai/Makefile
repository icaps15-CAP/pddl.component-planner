
expdir = component-planner
link = exp

.PHONY: copy refresh clean

all: component-planner component-planner-cost \
	component-planner-cost-large component-planner-large

copy: $(link)/component-planner \
	$(link)/component-planner-cost \
	$(link)/component-planner-cost-large \
	$(link)/component-planner-large

$(link)/%: % $(link)
	cp -v $< $@

$(link):
	ln -s $(HOME)/repos/$(expdir) $(link)

refresh:
	sbcl --quit \
	--eval "(ql:register-local-projects)" \
	--eval '(push :interpret-pddl *features*)' \
	--eval '(ql:quickload :pddl.component-planner.experiment)'

component-planner: refresh
	sbcl --dynamic-space-size 1900 \
	--disable-debugger \
	--eval '(push :interpret-pddl *features*)' \
	--eval '(ql:quickload :pddl.component-planner.experiment)' \
	--eval '(pddl.component-planner.experiment::save "component-planner")'
	chmod +x $@

component-planner-cost: refresh
	sbcl --dynamic-space-size 1900 \
	--disable-debugger \
	--eval '(push :interpret-pddl *features*)' \
	--eval '(push :add-cost *features*)' \
	--eval '(ql:quickload :pddl.component-planner.experiment)' \
	--eval '(pddl.component-planner.experiment::save "component-planner-cost")'
	chmod +x $@

component-planner-large: refresh
	sbcl --dynamic-space-size 8000 \
	--disable-debugger \
	--eval '(push :interpret-pddl *features*)' \
	--eval '(ql:quickload :pddl.component-planner.experiment)' \
	--eval '(pddl.component-planner.experiment::save "component-planner-large")'
	chmod +x $@

component-planner-cost-large: refresh
	sbcl --dynamic-space-size 8000 \
	--disable-debugger \
	--eval '(push :interpret-pddl *features*)' \
	--eval '(push :add-cost *features*)' \
	--eval '(ql:quickload :pddl.component-planner.experiment)' \
	--eval '(pddl.component-planner.experiment::save "component-planner-cost-large")'
	chmod +x $@

clean:
	rm -fv component-planner* $(link)/component-planner* $(link)
