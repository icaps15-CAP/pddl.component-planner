(DEFINE (PROBLEM DEPOTPROB1212)
 (:DOMAIN DEPOT)
 (:OBJECTS DEPOT0 - PLACE
           DISTRIBUTOR0 - PLACE
           DISTRIBUTOR1 - PLACE
           TRUCK0 - TRUCK
           TRUCK1 - TRUCK
           PALLET0 - SURFACE
           PALLET1 - SURFACE
           PALLET2 - SURFACE
           CRATE0 - SURFACE
           CRATE1 - SURFACE
           CRATE2 - SURFACE
           CRATE3 - SURFACE
           CRATE4 - SURFACE
           CRATE5 - SURFACE
           CRATE6 - SURFACE
           CRATE7 - SURFACE
           CRATE8 - SURFACE
           CRATE9 - SURFACE
           HOIST0 - HOIST
           HOIST1 - HOIST
           HOIST2 - HOIST)

 (:INIT
  (PALLET PALLET0)
  (AT PALLET0 DEPOT0)
  (CLEAR CRATE4)
  (PALLET PALLET1)
  (AT PALLET1 DISTRIBUTOR0)
  (CLEAR CRATE8)
  (PALLET PALLET2)
  (AT PALLET2 DISTRIBUTOR1)
  (CLEAR CRATE9)
  (AT TRUCK0 DEPOT0)
  (AT TRUCK1 DISTRIBUTOR0)
  (AT HOIST0 DEPOT0)
  (AVAILABLE HOIST0)
  (AT HOIST1 DISTRIBUTOR0)
  (AVAILABLE HOIST1)
  (AT HOIST2 DISTRIBUTOR1)
  (AVAILABLE HOIST2)
  (CRATE CRATE0)
  (AT CRATE0 DISTRIBUTOR1)
  (ON CRATE0 PALLET2)
  (CRATE CRATE1)
  (AT CRATE1 DEPOT0)
  (ON CRATE1 PALLET0)
  (CRATE CRATE2)
  (AT CRATE2 DISTRIBUTOR1)
  (ON CRATE2 CRATE0)
  (CRATE CRATE3)
  (AT CRATE3 DEPOT0)
  (ON CRATE3 CRATE1)
  (CRATE CRATE4)
  (AT CRATE4 DEPOT0)
  (ON CRATE4 CRATE3)
  (CRATE CRATE5)
  (AT CRATE5 DISTRIBUTOR1)
  (ON CRATE5 CRATE2)
  (CRATE CRATE6)
  (AT CRATE6 DISTRIBUTOR0)
  (ON CRATE6 PALLET1)
  (CRATE CRATE7)
  (AT CRATE7 DISTRIBUTOR0)
  (ON CRATE7 CRATE6)
  (CRATE CRATE8)
  (AT CRATE8 DISTRIBUTOR0)
  (ON CRATE8 CRATE7)
  (CRATE CRATE9)
  (AT CRATE9 DISTRIBUTOR1)
  (ON CRATE9 CRATE5))
 (:GOAL
  (AND (ON CRATE0 CRATE5)
       (ON CRATE1 PALLET1)
       (ON CRATE2 CRATE0)
       (ON CRATE3 PALLET2)
       (ON CRATE4 CRATE6)
       (ON CRATE5 CRATE4)
       (ON CRATE6 CRATE9)
       (ON CRATE7 CRATE1)
       (ON CRATE8 CRATE3)
       (ON CRATE9 PALLET0))))