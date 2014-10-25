(DEFINE (PROBLEM DEPOTPROB8765)
 (:DOMAIN DEPOT)
 (:OBJECTS DEPOT0 - PLACE
           DEPOT1 - PLACE
           DEPOT2 - PLACE
           DISTRIBUTOR0 - PLACE
           DISTRIBUTOR1 - PLACE
           DISTRIBUTOR2 - PLACE
           TRUCK0 - TRUCK
           TRUCK1 - TRUCK
           PALLET0 - SURFACE
           PALLET1 - SURFACE
           PALLET2 - SURFACE
           PALLET3 - SURFACE
           PALLET4 - SURFACE
           PALLET5 - SURFACE
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
           HOIST2 - HOIST
           HOIST3 - HOIST
           HOIST4 - HOIST
           HOIST5 - HOIST)

 (:INIT
  (PALLET PALLET0)
  (AT PALLET0 DEPOT0)
  (CLEAR CRATE1)
  (PALLET PALLET1)
  (AT PALLET1 DEPOT1)
  (CLEAR CRATE3)
  (PALLET PALLET2)
  (AT PALLET2 DEPOT2)
  (CLEAR CRATE9)
  (PALLET PALLET3)
  (AT PALLET3 DISTRIBUTOR0)
  (CLEAR PALLET3)
  (PALLET PALLET4)
  (AT PALLET4 DISTRIBUTOR1)
  (CLEAR PALLET4)
  (PALLET PALLET5)
  (AT PALLET5 DISTRIBUTOR2)
  (CLEAR CRATE8)
  (AT TRUCK0 DEPOT2)
  (AT TRUCK1 DISTRIBUTOR0)
  (AT HOIST0 DEPOT0)
  (AVAILABLE HOIST0)
  (AT HOIST1 DEPOT1)
  (AVAILABLE HOIST1)
  (AT HOIST2 DEPOT2)
  (AVAILABLE HOIST2)
  (AT HOIST3 DISTRIBUTOR0)
  (AVAILABLE HOIST3)
  (AT HOIST4 DISTRIBUTOR1)
  (AVAILABLE HOIST4)
  (AT HOIST5 DISTRIBUTOR2)
  (AVAILABLE HOIST5)
  (CRATE CRATE0)
  (AT CRATE0 DEPOT1)
  (ON CRATE0 PALLET1)
  (CRATE CRATE1)
  (AT CRATE1 DEPOT0)
  (ON CRATE1 PALLET0)
  (CRATE CRATE2)
  (AT CRATE2 DEPOT2)
  (ON CRATE2 PALLET2)
  (CRATE CRATE3)
  (AT CRATE3 DEPOT1)
  (ON CRATE3 CRATE0)
  (CRATE CRATE4)
  (AT CRATE4 DEPOT2)
  (ON CRATE4 CRATE2)
  (CRATE CRATE5)
  (AT CRATE5 DEPOT2)
  (ON CRATE5 CRATE4)
  (CRATE CRATE6)
  (AT CRATE6 DISTRIBUTOR2)
  (ON CRATE6 PALLET5)
  (CRATE CRATE7)
  (AT CRATE7 DISTRIBUTOR2)
  (ON CRATE7 CRATE6)
  (CRATE CRATE8)
  (AT CRATE8 DISTRIBUTOR2)
  (ON CRATE8 CRATE7)
  (CRATE CRATE9)
  (AT CRATE9 DEPOT2)
  (ON CRATE9 CRATE5))
 (:GOAL
  (AND (ON CRATE0 CRATE7)
       (ON CRATE1 PALLET4)
       (ON CRATE2 PALLET5)
       (ON CRATE3 CRATE9)
       (ON CRATE4 PALLET0)
       (ON CRATE5 PALLET2)
       (ON CRATE6 CRATE5)
       (ON CRATE7 CRATE1)
       (ON CRATE8 PALLET3)
       (ON CRATE9 CRATE2))))