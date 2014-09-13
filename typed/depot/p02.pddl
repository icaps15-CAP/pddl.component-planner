(DEFINE (PROBLEM DEPOTPROB7512)
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
           HOIST0 - HOIST
           HOIST1 - HOIST
           HOIST2 - HOIST)

 (:INIT
  (PALLET PALLET0)
  (AT PALLET0 DEPOT0)
  (CLEAR CRATE0)
  (PALLET PALLET1)
  (AT PALLET1 DISTRIBUTOR0)
  (CLEAR CRATE3)
  (PALLET PALLET2)
  (AT PALLET2 DISTRIBUTOR1)
  (CLEAR CRATE2)
  (AT TRUCK0 DEPOT0)
  (AT TRUCK1 DEPOT0)
  (AT HOIST0 DEPOT0)
  (AVAILABLE HOIST0)
  (AT HOIST1 DISTRIBUTOR0)
  (AVAILABLE HOIST1)
  (AT HOIST2 DISTRIBUTOR1)
  (AVAILABLE HOIST2)
  (CRATE CRATE0)
  (AT CRATE0 DEPOT0)
  (ON CRATE0 PALLET0)
  (CRATE CRATE1)
  (AT CRATE1 DISTRIBUTOR1)
  (ON CRATE1 PALLET2)
  (CRATE CRATE2)
  (AT CRATE2 DISTRIBUTOR1)
  (ON CRATE2 CRATE1)
  (CRATE CRATE3)
  (AT CRATE3 DISTRIBUTOR0)
  (ON CRATE3 PALLET1))
 (:GOAL
  (AND (ON CRATE0 PALLET2)
       (ON CRATE1 CRATE3)
       (ON CRATE2 PALLET0)
       (ON CRATE3 PALLET1))))