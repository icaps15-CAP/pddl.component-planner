(DEFINE (PROBLEM DEPOTPROB7615)
 (:DOMAIN DEPOT)
 (:OBJECTS DEPOT0 - PLACE
           DEPOT1 - PLACE
           DEPOT2 - PLACE
           DEPOT3 - PLACE
           DISTRIBUTOR0 - PLACE
           DISTRIBUTOR1 - PLACE
           DISTRIBUTOR2 - PLACE
           DISTRIBUTOR3 - PLACE
           TRUCK0 - TRUCK
           TRUCK1 - TRUCK
           TRUCK2 - TRUCK
           TRUCK3 - TRUCK
           PALLET0 - SURFACE
           PALLET1 - SURFACE
           PALLET2 - SURFACE
           PALLET3 - SURFACE
           PALLET4 - SURFACE
           PALLET5 - SURFACE
           PALLET6 - SURFACE
           PALLET7 - SURFACE
           PALLET8 - SURFACE
           PALLET9 - SURFACE
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
           HOIST5 - HOIST
           HOIST6 - HOIST
           HOIST7 - HOIST)

 (:INIT
  (PALLET PALLET0)
  (AT PALLET0 DEPOT0)
  (CLEAR CRATE13)
  (PALLET PALLET1)
  (AT PALLET1 DEPOT1)
  (CLEAR CRATE14)
  (PALLET PALLET2)
  (AT PALLET2 DEPOT2)
  (CLEAR PALLET2)
  (PALLET PALLET3)
  (AT PALLET3 DEPOT3)
  (CLEAR CRATE5)
  (PALLET PALLET4)
  (AT PALLET4 DISTRIBUTOR0)
  (CLEAR PALLET4)
  (PALLET PALLET5)
  (AT PALLET5 DISTRIBUTOR1)
  (CLEAR CRATE9)
  (PALLET PALLET6)
  (AT PALLET6 DISTRIBUTOR2)
  (CLEAR CRATE8)
  (PALLET PALLET7)
  (AT PALLET7 DISTRIBUTOR3)
  (CLEAR CRATE10)
  (PALLET PALLET8)
  (AT PALLET8 DEPOT1)
  (CLEAR CRATE11)
  (PALLET PALLET9)
  (AT PALLET9 DEPOT2)
  (CLEAR PALLET9)
  (AT TRUCK0 DISTRIBUTOR2)
  (AT TRUCK1 DEPOT0)
  (AT TRUCK2 DEPOT1)
  (AT TRUCK3 DISTRIBUTOR1)
  (AT HOIST0 DEPOT0)
  (AVAILABLE HOIST0)
  (AT HOIST1 DEPOT1)
  (AVAILABLE HOIST1)
  (AT HOIST2 DEPOT2)
  (AVAILABLE HOIST2)
  (AT HOIST3 DEPOT3)
  (AVAILABLE HOIST3)
  (AT HOIST4 DISTRIBUTOR0)
  (AVAILABLE HOIST4)
  (AT HOIST5 DISTRIBUTOR1)
  (AVAILABLE HOIST5)
  (AT HOIST6 DISTRIBUTOR2)
  (AVAILABLE HOIST6)
  (AT HOIST7 DISTRIBUTOR3)
  (AVAILABLE HOIST7)
  (CRATE CRATE0)
  (AT CRATE0 DISTRIBUTOR3)
  (ON CRATE0 PALLET7)
  (CRATE CRATE1)
  (AT CRATE1 DISTRIBUTOR1)
  (ON CRATE1 PALLET5)
  (CRATE CRATE2)
  (AT CRATE2 DEPOT3)
  (ON CRATE2 PALLET3)
  (CRATE CRATE3)
  (AT CRATE3 DEPOT0)
  (ON CRATE3 PALLET0)
  (CRATE CRATE4)
  (AT CRATE4 DEPOT0)
  (ON CRATE4 CRATE3)
  (CRATE CRATE5)
  (AT CRATE5 DEPOT3)
  (ON CRATE5 CRATE2)
  (CRATE CRATE6)
  (AT CRATE6 DEPOT1)
  (ON CRATE6 PALLET1)
  (CRATE CRATE7)
  (AT CRATE7 DISTRIBUTOR2)
  (ON CRATE7 PALLET6)
  (CRATE CRATE8)
  (AT CRATE8 DISTRIBUTOR2)
  (ON CRATE8 CRATE7)
  (CRATE CRATE9)
  (AT CRATE9 DISTRIBUTOR1)
  (ON CRATE9 CRATE1)
  (CRATE CRATE10)
  (AT CRATE10 DISTRIBUTOR3)
  (ON CRATE10 CRATE0)
  (CRATE CRATE11)
  (AT CRATE11 DEPOT1)
  (ON CRATE11 PALLET8)
  (CRATE CRATE12)
  (AT CRATE12 DEPOT1)
  (ON CRATE12 CRATE6)
  (CRATE CRATE13)
  (AT CRATE13 DEPOT0)
  (ON CRATE13 CRATE4)
  (CRATE CRATE14)
  (AT CRATE14 DEPOT1)
  (ON CRATE14 CRATE12))
 (:GOAL
  (AND (ON CRATE0 PALLET3)
       (ON CRATE1 CRATE11)
       (ON CRATE2 PALLET6)
       (ON CRATE3 CRATE0)
       (ON CRATE4 CRATE5)
       (ON CRATE5 CRATE14)
       (ON CRATE6 PALLET4)
       (ON CRATE7 PALLET2)
       (ON CRATE8 PALLET7)
       (ON CRATE9 CRATE8)
       (ON CRATE11 PALLET5)
       (ON CRATE12 CRATE6)
       (ON CRATE13 CRATE2)
       (ON CRATE14 PALLET1))))