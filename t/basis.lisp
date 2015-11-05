(in-package :pddl.component-planner.test)
(in-suite :pddl.component-planner)

(define (domain assemble)
  (:requirements :strips :action-costs :typing)
  (:types machine step product part)
  (:predicates (available ?m - machine)
               (use ?m - machine ?s - step)
               (next ?s1 ?s2 - step)
               (notmaking ?p - product)
               (making ?m - machine ?p - product)
               (made ?p - product ?step - step)
               (assemble ?prod - product ?part - part ?s - step))
  (:functions (total-cost)
              (span ?p - product ?s - step))
  (:action start
           :parameters (?p - product ?s - step ?m - machine)
           :precondition (and (available ?m) (use ?m ?s) (notmaking ?p))
           :effect (and (making ?m ?p)
                        (not (notmaking ?p))
                        (not (available ?m))
                        (increase (total-cost) 1)))
  (:action assemble
           :parameters (?p - product ?s1 ?s2 - step ?m - machine
                           ?part - part)
           :precondition (and (making ?m ?p)
                              (made ?p ?s1)
                              (next ?s1 ?s2)
                              (assemble ?p ?part ?s2))
           :effect (and (not (made ?p ?s1)) (made ?p ?s2)
                        (increase (total-cost) (span ?p ?s2))))
  (:action end
           :parameters (?p - product ?s - step ?m - machine)
           :precondition (and (making ?m ?p) (made ?p ?s))
           :effect (and (not (making ?m ?p))
                        (notmaking ?p)
                        (available ?m)
                        (increase (total-cost) 1))))

(define (problem assemblep)
  (:domain assemble)
  (:objects m1 m2 - machine
            p1 p2 - product
            s0 s1 s2 - step
            pa11 pa12 pa21 pa22 - part)
  (:init (made p1 s0)
         (made p2 s0)
         (notmaking p1)
         (notmaking p2)
         (available m1)
         (available m2)
         (next s0 s1)
         (next s1 s2)
         (use m1 s1)
         (use m2 s2)
         (assemble p1 pa11 s1)
         (assemble p1 pa12 s2)
         (assemble p2 pa21 s1)
         (assemble p2 pa22 s2)
         (= (total-cost) 0)
         (= (span p1 s0) 0)
         (= (span p1 s1) 3)
         (= (span p1 s2) 5)
         (= (span p2 s0) 0)
         (= (span p2 s1) 2)
         (= (span p2 s2) 3))
  (:goal (and (made p1 s2) (made p2 s2)
              (notmaking p1) (notmaking p2)))
  (:metric minimize (total-cost)))


(define (domain assemble-nocost)
  (:requirements :strips :typing)
  (:types machine step product part)
  (:predicates (available ?m - machine)
               (use ?m - machine ?s - step)
               (next ?s1 ?s2 - step)
               (notmaking ?p - product)
               (making ?m - machine ?p - product)
               (made ?p - product ?step - step)
               (assemble ?prod - product ?part - part ?s - step))
  (:action start
           :parameters (?p - product ?s - step ?m - machine)
           :precondition (and (available ?m) (use ?m ?s) (notmaking ?p))
           :effect (and (making ?m ?p)
                        (not (notmaking ?p))
                        (not (available ?m))))
  (:action assemble
           :parameters (?p - product ?s1 ?s2 - step ?m - machine
                           ?part - part)
           :precondition (and (making ?m ?p)
                              (made ?p ?s1)
                              (next ?s1 ?s2)
                              (assemble ?p ?part ?s2))
           :effect (and (not (made ?p ?s1)) (made ?p ?s2)))
  (:action end
           :parameters (?p - product ?s - step ?m - machine)
           :precondition (and (making ?m ?p) (made ?p ?s))
           :effect (and (not (making ?m ?p))
                        (notmaking ?p)
                        (available ?m))))

(define (problem assemblep-nocost)
  (:domain assemble-nocost)
  (:objects m1 m2 - machine
            p1 p2 - product
            s0 s1 s2 - step
            pa11 pa12 pa21 pa22 - part)
  (:init (made p1 s0)
         (made p2 s0)
         (notmaking p1)
         (notmaking p2)
         (available m1)
         (available m2)
         (next s0 s1)
         (next s1 s2)
         (use m1 s1)
         (use m2 s2)
         (assemble p1 pa11 s1)
         (assemble p1 pa12 s2)
         (assemble p2 pa21 s1)
         (assemble p2 pa22 s2))
  (:goal (and (made p1 s2) (made p2 s2)
              (notmaking p1) (notmaking p2))))

(define (domain assemble-resource)
  (:requirements :strips :action-costs :typing)
  (:types machine step product part resource)
  (:predicates (available ?m - machine)
               (use ?m - machine ?s - step)
               (next ?s1 ?s2 - step)
               (notmaking ?p - product)
               (making ?m - machine ?p - product)
               (made ?p - product ?step - step)
               (assemble ?prod - product ?part - part ?s - step)
               (fresh ?r - resource))
  (:functions (total-cost)
              (span ?p - product ?s - step))
  (:action start
           :parameters (?p - product ?s - step ?m - machine)
           :precondition (and (available ?m) (use ?m ?s) (notmaking ?p))
           :effect (and (making ?m ?p)
                        (not (notmaking ?p))
                        (not (available ?m))
                        (increase (total-cost) 1)))
  (:action assemble
           :parameters (?p - product ?s1 ?s2 - step ?m - machine
                           ?part - part ?r - resource)
           :precondition (and (making ?m ?p)
                              (fresh ?r)
                              (made ?p ?s1)
                              (next ?s1 ?s2)
                              (assemble ?p ?part ?s2))
           :effect (and (not (made ?p ?s1)) (made ?p ?s2)
                        (not (fresh ?r))
                        (increase (total-cost) (span ?p ?s2))))
  (:action end
           :parameters (?p - product ?s - step ?m - machine)
           :precondition (and (making ?m ?p) (made ?p ?s))
           :effect (and (not (making ?m ?p))
                        (notmaking ?p)
                        (available ?m)
                        (increase (total-cost) 1)))
  (:action supply
           :parameters (?r - resource)
           :precondition (and)
           :effect (fresh ?r)))

(define (problem assemblep-resource)
  (:domain assemble-resource)
  (:objects m1 m2 - machine
            p1 p2 - product
            s0 s1 s2 - step
            pa11 pa12 pa21 pa22 - part
            r - resource)
  (:init (fresh r)
         (made p1 s0)
         (made p2 s0)
         (notmaking p1)
         (notmaking p2)
         (available m1)
         (available m2)
         (next s0 s1)
         (next s1 s2)
         (use m1 s1)
         (use m2 s2)
         (assemble p1 pa11 s1)
         (assemble p1 pa12 s2)
         (assemble p2 pa21 s1)
         (assemble p2 pa22 s2)
         (= (total-cost) 0)
         (= (span p1 s0) 0)
         (= (span p1 s1) 3)
         (= (span p1 s2) 5)
         (= (span p2 s0) 0)
         (= (span p2 s1) 2)
         (= (span p2 s2) 3))
  (:goal (and (made p1 s2) (made p2 s2)
              (notmaking p1) (notmaking p2)))
  (:metric minimize (total-cost)))
