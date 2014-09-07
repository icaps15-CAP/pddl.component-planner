
small = 1900
large = 7500
expdir = aaai15exp
link = exp

.PHONY: copy refresh clean

all: component-planner

copy: $(link)/component-planner

$(link)/%: % $(link)
	cp -v $< $@

$(link):
	ln -s $(HOME)/repos/$(expdir) $(link)

cost 	  = --eval '(push :add-cost *features*)'

interpret = --eval '(push :interpret-pddl *features*)'
load      = --eval '(ql:quickload :pddl.component-planner.experiment)'
save      = --eval '(pddl.component-planner.experiment::save "$@")'
common    = $(interpret) $(load) $(save)

refresh:
	sbcl --non-interactive \
	--eval "(progn (print :register-local-projects)(ql:register-local-projects))" \
	$(interpret) $(cost) $(load)

component-planner: # refresh # not required anymore
	sbcl --dynamic-space-size $(small) --non-interactive $(common)
	chmod +x $@

clean:
	rm -fv component-planner* $(link)/component-planner* $(link)
	find -name "*.log" -delete
	find -name "*.plan*" -delete
	find -name "*.cost" -delete
	find -name "*.err" -delete
	find -name "*.time" -delete
	find -name "*.sas*" -delete

