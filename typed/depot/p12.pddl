(DEFINE (PROBLEM DEPOTPROB9876)
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
           CRATE10 - SURFACE
           CRATE11 - SURFACE
           CRATE12 - SURFACE
           CRATE13 - SURFACE
           CRATE14 - SURFACE
           HOIST0 - HOIST
           HOIST1 - HOIST
           HOIST2 - HOIST
           HOIST3 - HOIST
           HOIST4 - HOIST
           HOIST5 - HOIST)

 (:INIT
  (PALLET PALLET0)
  (AT PALLET0 DEPOT0)
  (CLEAR PALLET0)
  (PALLET PALLET1)
  (AT PALLET1 DEPOT1)
  (CLEAR CRATE12)
  (PALLET PALLET2)
  (AT PALLET2 DEPOT2)
  (CLEAR PALLET2)
  (PALLET PALLET3)
  (AT PALLET3 DISTRIBUTOR0)
  (CLEAR CRATE4)
  (PALLET PALLET4)
  (AT PALLET4 DISTRIBUTOR1)
  (CLEAR CRATE14)
  (PALLET PALLET5)
  (AT PALLET5 DISTRIBUTOR2)
  (CLEAR CRATE13)
  (AT TRUCK0 DISTRIBUTOR1)
  (AT TRUCK1 DEPOT1)
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
  (AT CRATE0 DISTRIBUTOR2)
  (ON CRATE0 PALLET5)
  (CRATE CRATE1)
  (AT CRATE1 DEPOT1)
  (ON CRATE1 PALLET1)
  (CRATE CRATE2)
  (AT CRATE2 DISTRIBUTOR0)
  (ON CRATE2 PALLET3)
  (CRATE CRATE3)
  (AT CRATE3 DISTRIBUTOR2)
  (ON CRATE3 CRATE0)
  (CRATE CRATE4)
  (AT CRATE4 DISTRIBUTOR0)
  (ON CRATE4 CRATE2)
  (CRATE CRATE5)
  (AT CRATE5 DEPOT1)
  (ON CRATE5 CRATE1)
  (CRATE CRATE6)
  (AT CRATE6 DISTRIBUTOR2)
  (ON CRATE6 CRATE3)
  (CRATE CRATE7)
  (AT CRATE7 DISTRIBUTOR2)
  (ON CRATE7 CRATE6)
  (CRATE CRATE8)
  (AT CRATE8 DISTRIBUTOR2)
  (ON CRATE8 CRATE7)
  (CRATE CRATE9)
  (AT CRATE9 DISTRIBUTOR2)
  (ON CRATE9 CRATE8)
  (CRATE CRATE10)
  (AT CRATE10 DEPOT1)
  (ON CRATE10 CRATE5)
  (CRATE CRATE11)
  (AT CRATE11 DISTRIBUTOR1)
  (ON CRATE11 PALLET4)
  (CRATE CRATE12)
  (AT CRATE12 DEPOT1)
  (ON CRATE12 CRATE10)
  (CRATE CRATE13)
  (AT CRATE13 DISTRIBUTOR2)
  (ON CRATE13 CRATE9)
  (CRATE CRATE14)
  (AT CRATE14 DISTRIBUTOR1)
  (ON CRATE14 CRATE11))
 (:GOAL
  (AND (ON CRATE0 PALLET4)
       (ON CRATE1 CRATE12)
       (ON CRATE2 CRATE0)
       (ON CRATE3 CRATE9)
       (ON CRATE5 PALLET0)
       (ON CRATE6 CRATE2)
       (ON CRATE9 PALLET2)
       (ON CRATE10 CRATE13)
       (ON CRATE12 PALLET5)
       (ON CRATE13 PALLET1)
       (ON CRATE14 CRATE10))))