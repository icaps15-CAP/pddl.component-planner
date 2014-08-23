
small = 1900
large = 7500
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

cost 	  = --eval '(push :add-cost *features*)'

interpret = --eval '(push :interpret-pddl *features*)'
load      = --eval '(ql:quickload :pddl.component-planner.experiment)'
save      = --eval '(pddl.component-planner.experiment::save "$@")'
common    = $(interpret) $(load) $(save)

refresh:
	sbcl --quit \
	--eval "(ql:register-local-projects)" \
	$(interpret) $(cost) $(load)

component-planner: refresh
	sbcl --dynamic-space-size $(small) \
	--disable-debugger $(common)
	chmod +x $@

component-planner-cost: refresh
	sbcl --dynamic-space-size $(small) \
	--disable-debugger $(cost) $(common)
	chmod +x $@

component-planner-large: refresh
	sbcl --dynamic-space-size $(large) \
	--disable-debugger $(common)
	chmod +x $@

component-planner-cost-large: refresh
	sbcl --dynamic-space-size $(large) \
	--disable-debugger $(cost) $(common)
	chmod +x $@

clean:
	rm -fv component-planner* $(link)/component-planner* $(link)
