(DEFINE (PROBLEM DEPOTPROB1935)
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
           HOIST0 - HOIST
           HOIST1 - HOIST
           HOIST2 - HOIST)

 (:INIT
  (PALLET PALLET0)
  (AT PALLET0 DEPOT0)
  (CLEAR CRATE1)
  (PALLET PALLET1)
  (AT PALLET1 DISTRIBUTOR0)
  (CLEAR CRATE4)
  (PALLET PALLET2)
  (AT PALLET2 DISTRIBUTOR1)
  (CLEAR CRATE5)
  (AT TRUCK0 DEPOT0)
  (AT TRUCK1 DISTRIBUTOR0)
  (AT HOIST0 DEPOT0)
  (AVAILABLE HOIST0)
  (AT HOIST1 DISTRIBUTOR0)
  (AVAILABLE HOIST1)
  (AT HOIST2 DISTRIBUTOR1)
  (AVAILABLE HOIST2)
  (CRATE CRATE0)
  (AT CRATE0 DISTRIBUTOR0)
  (ON CRATE0 PALLET1)
  (CRATE CRATE1)
  (AT CRATE1 DEPOT0)
  (ON CRATE1 PALLET0)
  (CRATE CRATE2)
  (AT CRATE2 DISTRIBUTOR1)
  (ON CRATE2 PALLET2)
  (CRATE CRATE3)
  (AT CRATE3 DISTRIBUTOR0)
  (ON CRATE3 CRATE0)
  (CRATE CRATE4)
  (AT CRATE4 DISTRIBUTOR0)
  (ON CRATE4 CRATE3)
  (CRATE CRATE5)
  (AT CRATE5 DISTRIBUTOR1)
  (ON CRATE5 CRATE2))
 (:GOAL
  (AND (ON CRATE0 CRATE1)
       (ON CRATE1 PALLET2)
       (ON CRATE2 PALLET0)
       (ON CRATE3 CRATE2)
       (ON CRATE4 PALLET1)
       (ON CRATE5 CRATE0))))