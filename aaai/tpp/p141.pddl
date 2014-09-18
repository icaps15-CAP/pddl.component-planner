; Domain designed by Alfonso Gerevini and Alessandro Saetti
; This file has been automatically generated by the generator available from
; http://zeus.ing.unibs.it/ipc-5/generators/index.html

(define (problem TPP)
(:domain TPP-Propositional)
(:objects
	goods1 goods2 goods3 goods4 goods5 goods6 goods7 goods8 goods9 goods10 goods11 goods12 goods13 goods14 goods15 goods16 goods17 goods18 goods19 goods20 - goods
	truck1 truck2 truck3 truck4 truck5 truck6 truck7 truck8 truck9 truck10 - truck
	market1 market2 market3 market4 market5 market6 market7 market8 market9 market10 market11 market12 market13 market14 market15 - market
	depot1 depot2 depot3 depot4 depot5 depot6 depot7 depot8 depot9 depot10 depot11 depot12 depot13 depot14 depot15 - depot
	level0 level1 level2 level3 level4 level5 level6 level7 level8 level9 level10 level11 level12 level13 level14 level15 level16 level17 level18 level19 level20 - level)

(:init
	(next level1 level0)
	(next level2 level1)
	(next level3 level2)
	(next level4 level3)
	(next level5 level4)
	(next level6 level5)
	(next level7 level6)
	(next level8 level7)
	(next level9 level8)
	(next level10 level9)
	(next level11 level10)
	(next level12 level11)
	(next level13 level12)
	(next level14 level13)
	(next level15 level14)
	(next level16 level15)
	(next level17 level16)
	(next level18 level17)
	(next level19 level18)
	(next level20 level19)
	(ready-to-load goods1 market1 level0)
	(ready-to-load goods1 market2 level0)
	(ready-to-load goods1 market3 level0)
	(ready-to-load goods1 market4 level0)
	(ready-to-load goods1 market5 level0)
	(ready-to-load goods1 market6 level0)
	(ready-to-load goods1 market7 level0)
	(ready-to-load goods1 market8 level0)
	(ready-to-load goods1 market9 level0)
	(ready-to-load goods1 market10 level0)
	(ready-to-load goods1 market11 level0)
	(ready-to-load goods1 market12 level0)
	(ready-to-load goods1 market13 level0)
	(ready-to-load goods1 market14 level0)
	(ready-to-load goods1 market15 level0)
	(ready-to-load goods2 market1 level0)
	(ready-to-load goods2 market2 level0)
	(ready-to-load goods2 market3 level0)
	(ready-to-load goods2 market4 level0)
	(ready-to-load goods2 market5 level0)
	(ready-to-load goods2 market6 level0)
	(ready-to-load goods2 market7 level0)
	(ready-to-load goods2 market8 level0)
	(ready-to-load goods2 market9 level0)
	(ready-to-load goods2 market10 level0)
	(ready-to-load goods2 market11 level0)
	(ready-to-load goods2 market12 level0)
	(ready-to-load goods2 market13 level0)
	(ready-to-load goods2 market14 level0)
	(ready-to-load goods2 market15 level0)
	(ready-to-load goods3 market1 level0)
	(ready-to-load goods3 market2 level0)
	(ready-to-load goods3 market3 level0)
	(ready-to-load goods3 market4 level0)
	(ready-to-load goods3 market5 level0)
	(ready-to-load goods3 market6 level0)
	(ready-to-load goods3 market7 level0)
	(ready-to-load goods3 market8 level0)
	(ready-to-load goods3 market9 level0)
	(ready-to-load goods3 market10 level0)
	(ready-to-load goods3 market11 level0)
	(ready-to-load goods3 market12 level0)
	(ready-to-load goods3 market13 level0)
	(ready-to-load goods3 market14 level0)
	(ready-to-load goods3 market15 level0)
	(ready-to-load goods4 market1 level0)
	(ready-to-load goods4 market2 level0)
	(ready-to-load goods4 market3 level0)
	(ready-to-load goods4 market4 level0)
	(ready-to-load goods4 market5 level0)
	(ready-to-load goods4 market6 level0)
	(ready-to-load goods4 market7 level0)
	(ready-to-load goods4 market8 level0)
	(ready-to-load goods4 market9 level0)
	(ready-to-load goods4 market10 level0)
	(ready-to-load goods4 market11 level0)
	(ready-to-load goods4 market12 level0)
	(ready-to-load goods4 market13 level0)
	(ready-to-load goods4 market14 level0)
	(ready-to-load goods4 market15 level0)
	(ready-to-load goods5 market1 level0)
	(ready-to-load goods5 market2 level0)
	(ready-to-load goods5 market3 level0)
	(ready-to-load goods5 market4 level0)
	(ready-to-load goods5 market5 level0)
	(ready-to-load goods5 market6 level0)
	(ready-to-load goods5 market7 level0)
	(ready-to-load goods5 market8 level0)
	(ready-to-load goods5 market9 level0)
	(ready-to-load goods5 market10 level0)
	(ready-to-load goods5 market11 level0)
	(ready-to-load goods5 market12 level0)
	(ready-to-load goods5 market13 level0)
	(ready-to-load goods5 market14 level0)
	(ready-to-load goods5 market15 level0)
	(ready-to-load goods6 market1 level0)
	(ready-to-load goods6 market2 level0)
	(ready-to-load goods6 market3 level0)
	(ready-to-load goods6 market4 level0)
	(ready-to-load goods6 market5 level0)
	(ready-to-load goods6 market6 level0)
	(ready-to-load goods6 market7 level0)
	(ready-to-load goods6 market8 level0)
	(ready-to-load goods6 market9 level0)
	(ready-to-load goods6 market10 level0)
	(ready-to-load goods6 market11 level0)
	(ready-to-load goods6 market12 level0)
	(ready-to-load goods6 market13 level0)
	(ready-to-load goods6 market14 level0)
	(ready-to-load goods6 market15 level0)
	(ready-to-load goods7 market1 level0)
	(ready-to-load goods7 market2 level0)
	(ready-to-load goods7 market3 level0)
	(ready-to-load goods7 market4 level0)
	(ready-to-load goods7 market5 level0)
	(ready-to-load goods7 market6 level0)
	(ready-to-load goods7 market7 level0)
	(ready-to-load goods7 market8 level0)
	(ready-to-load goods7 market9 level0)
	(ready-to-load goods7 market10 level0)
	(ready-to-load goods7 market11 level0)
	(ready-to-load goods7 market12 level0)
	(ready-to-load goods7 market13 level0)
	(ready-to-load goods7 market14 level0)
	(ready-to-load goods7 market15 level0)
	(ready-to-load goods8 market1 level0)
	(ready-to-load goods8 market2 level0)
	(ready-to-load goods8 market3 level0)
	(ready-to-load goods8 market4 level0)
	(ready-to-load goods8 market5 level0)
	(ready-to-load goods8 market6 level0)
	(ready-to-load goods8 market7 level0)
	(ready-to-load goods8 market8 level0)
	(ready-to-load goods8 market9 level0)
	(ready-to-load goods8 market10 level0)
	(ready-to-load goods8 market11 level0)
	(ready-to-load goods8 market12 level0)
	(ready-to-load goods8 market13 level0)
	(ready-to-load goods8 market14 level0)
	(ready-to-load goods8 market15 level0)
	(ready-to-load goods9 market1 level0)
	(ready-to-load goods9 market2 level0)
	(ready-to-load goods9 market3 level0)
	(ready-to-load goods9 market4 level0)
	(ready-to-load goods9 market5 level0)
	(ready-to-load goods9 market6 level0)
	(ready-to-load goods9 market7 level0)
	(ready-to-load goods9 market8 level0)
	(ready-to-load goods9 market9 level0)
	(ready-to-load goods9 market10 level0)
	(ready-to-load goods9 market11 level0)
	(ready-to-load goods9 market12 level0)
	(ready-to-load goods9 market13 level0)
	(ready-to-load goods9 market14 level0)
	(ready-to-load goods9 market15 level0)
	(ready-to-load goods10 market1 level0)
	(ready-to-load goods10 market2 level0)
	(ready-to-load goods10 market3 level0)
	(ready-to-load goods10 market4 level0)
	(ready-to-load goods10 market5 level0)
	(ready-to-load goods10 market6 level0)
	(ready-to-load goods10 market7 level0)
	(ready-to-load goods10 market8 level0)
	(ready-to-load goods10 market9 level0)
	(ready-to-load goods10 market10 level0)
	(ready-to-load goods10 market11 level0)
	(ready-to-load goods10 market12 level0)
	(ready-to-load goods10 market13 level0)
	(ready-to-load goods10 market14 level0)
	(ready-to-load goods10 market15 level0)
	(ready-to-load goods11 market1 level0)
	(ready-to-load goods11 market2 level0)
	(ready-to-load goods11 market3 level0)
	(ready-to-load goods11 market4 level0)
	(ready-to-load goods11 market5 level0)
	(ready-to-load goods11 market6 level0)
	(ready-to-load goods11 market7 level0)
	(ready-to-load goods11 market8 level0)
	(ready-to-load goods11 market9 level0)
	(ready-to-load goods11 market10 level0)
	(ready-to-load goods11 market11 level0)
	(ready-to-load goods11 market12 level0)
	(ready-to-load goods11 market13 level0)
	(ready-to-load goods11 market14 level0)
	(ready-to-load goods11 market15 level0)
	(ready-to-load goods12 market1 level0)
	(ready-to-load goods12 market2 level0)
	(ready-to-load goods12 market3 level0)
	(ready-to-load goods12 market4 level0)
	(ready-to-load goods12 market5 level0)
	(ready-to-load goods12 market6 level0)
	(ready-to-load goods12 market7 level0)
	(ready-to-load goods12 market8 level0)
	(ready-to-load goods12 market9 level0)
	(ready-to-load goods12 market10 level0)
	(ready-to-load goods12 market11 level0)
	(ready-to-load goods12 market12 level0)
	(ready-to-load goods12 market13 level0)
	(ready-to-load goods12 market14 level0)
	(ready-to-load goods12 market15 level0)
	(ready-to-load goods13 market1 level0)
	(ready-to-load goods13 market2 level0)
	(ready-to-load goods13 market3 level0)
	(ready-to-load goods13 market4 level0)
	(ready-to-load goods13 market5 level0)
	(ready-to-load goods13 market6 level0)
	(ready-to-load goods13 market7 level0)
	(ready-to-load goods13 market8 level0)
	(ready-to-load goods13 market9 level0)
	(ready-to-load goods13 market10 level0)
	(ready-to-load goods13 market11 level0)
	(ready-to-load goods13 market12 level0)
	(ready-to-load goods13 market13 level0)
	(ready-to-load goods13 market14 level0)
	(ready-to-load goods13 market15 level0)
	(ready-to-load goods14 market1 level0)
	(ready-to-load goods14 market2 level0)
	(ready-to-load goods14 market3 level0)
	(ready-to-load goods14 market4 level0)
	(ready-to-load goods14 market5 level0)
	(ready-to-load goods14 market6 level0)
	(ready-to-load goods14 market7 level0)
	(ready-to-load goods14 market8 level0)
	(ready-to-load goods14 market9 level0)
	(ready-to-load goods14 market10 level0)
	(ready-to-load goods14 market11 level0)
	(ready-to-load goods14 market12 level0)
	(ready-to-load goods14 market13 level0)
	(ready-to-load goods14 market14 level0)
	(ready-to-load goods14 market15 level0)
	(ready-to-load goods15 market1 level0)
	(ready-to-load goods15 market2 level0)
	(ready-to-load goods15 market3 level0)
	(ready-to-load goods15 market4 level0)
	(ready-to-load goods15 market5 level0)
	(ready-to-load goods15 market6 level0)
	(ready-to-load goods15 market7 level0)
	(ready-to-load goods15 market8 level0)
	(ready-to-load goods15 market9 level0)
	(ready-to-load goods15 market10 level0)
	(ready-to-load goods15 market11 level0)
	(ready-to-load goods15 market12 level0)
	(ready-to-load goods15 market13 level0)
	(ready-to-load goods15 market14 level0)
	(ready-to-load goods15 market15 level0)
	(ready-to-load goods16 market1 level0)
	(ready-to-load goods16 market2 level0)
	(ready-to-load goods16 market3 level0)
	(ready-to-load goods16 market4 level0)
	(ready-to-load goods16 market5 level0)
	(ready-to-load goods16 market6 level0)
	(ready-to-load goods16 market7 level0)
	(ready-to-load goods16 market8 level0)
	(ready-to-load goods16 market9 level0)
	(ready-to-load goods16 market10 level0)
	(ready-to-load goods16 market11 level0)
	(ready-to-load goods16 market12 level0)
	(ready-to-load goods16 market13 level0)
	(ready-to-load goods16 market14 level0)
	(ready-to-load goods16 market15 level0)
	(ready-to-load goods17 market1 level0)
	(ready-to-load goods17 market2 level0)
	(ready-to-load goods17 market3 level0)
	(ready-to-load goods17 market4 level0)
	(ready-to-load goods17 market5 level0)
	(ready-to-load goods17 market6 level0)
	(ready-to-load goods17 market7 level0)
	(ready-to-load goods17 market8 level0)
	(ready-to-load goods17 market9 level0)
	(ready-to-load goods17 market10 level0)
	(ready-to-load goods17 market11 level0)
	(ready-to-load goods17 market12 level0)
	(ready-to-load goods17 market13 level0)
	(ready-to-load goods17 market14 level0)
	(ready-to-load goods17 market15 level0)
	(ready-to-load goods18 market1 level0)
	(ready-to-load goods18 market2 level0)
	(ready-to-load goods18 market3 level0)
	(ready-to-load goods18 market4 level0)
	(ready-to-load goods18 market5 level0)
	(ready-to-load goods18 market6 level0)
	(ready-to-load goods18 market7 level0)
	(ready-to-load goods18 market8 level0)
	(ready-to-load goods18 market9 level0)
	(ready-to-load goods18 market10 level0)
	(ready-to-load goods18 market11 level0)
	(ready-to-load goods18 market12 level0)
	(ready-to-load goods18 market13 level0)
	(ready-to-load goods18 market14 level0)
	(ready-to-load goods18 market15 level0)
	(ready-to-load goods19 market1 level0)
	(ready-to-load goods19 market2 level0)
	(ready-to-load goods19 market3 level0)
	(ready-to-load goods19 market4 level0)
	(ready-to-load goods19 market5 level0)
	(ready-to-load goods19 market6 level0)
	(ready-to-load goods19 market7 level0)
	(ready-to-load goods19 market8 level0)
	(ready-to-load goods19 market9 level0)
	(ready-to-load goods19 market10 level0)
	(ready-to-load goods19 market11 level0)
	(ready-to-load goods19 market12 level0)
	(ready-to-load goods19 market13 level0)
	(ready-to-load goods19 market14 level0)
	(ready-to-load goods19 market15 level0)
	(ready-to-load goods20 market1 level0)
	(ready-to-load goods20 market2 level0)
	(ready-to-load goods20 market3 level0)
	(ready-to-load goods20 market4 level0)
	(ready-to-load goods20 market5 level0)
	(ready-to-load goods20 market6 level0)
	(ready-to-load goods20 market7 level0)
	(ready-to-load goods20 market8 level0)
	(ready-to-load goods20 market9 level0)
	(ready-to-load goods20 market10 level0)
	(ready-to-load goods20 market11 level0)
	(ready-to-load goods20 market12 level0)
	(ready-to-load goods20 market13 level0)
	(ready-to-load goods20 market14 level0)
	(ready-to-load goods20 market15 level0)
	(stored goods1 level0)
	(stored goods2 level0)
	(stored goods3 level0)
	(stored goods4 level0)
	(stored goods5 level0)
	(stored goods6 level0)
	(stored goods7 level0)
	(stored goods8 level0)
	(stored goods9 level0)
	(stored goods10 level0)
	(stored goods11 level0)
	(stored goods12 level0)
	(stored goods13 level0)
	(stored goods14 level0)
	(stored goods15 level0)
	(stored goods16 level0)
	(stored goods17 level0)
	(stored goods18 level0)
	(stored goods19 level0)
	(stored goods20 level0)
	(loaded goods1 truck1 level0)
	(loaded goods1 truck2 level0)
	(loaded goods1 truck3 level0)
	(loaded goods1 truck4 level0)
	(loaded goods1 truck5 level0)
	(loaded goods1 truck6 level0)
	(loaded goods1 truck7 level0)
	(loaded goods1 truck8 level0)
	(loaded goods1 truck9 level0)
	(loaded goods1 truck10 level0)
	(loaded goods2 truck1 level0)
	(loaded goods2 truck2 level0)
	(loaded goods2 truck3 level0)
	(loaded goods2 truck4 level0)
	(loaded goods2 truck5 level0)
	(loaded goods2 truck6 level0)
	(loaded goods2 truck7 level0)
	(loaded goods2 truck8 level0)
	(loaded goods2 truck9 level0)
	(loaded goods2 truck10 level0)
	(loaded goods3 truck1 level0)
	(loaded goods3 truck2 level0)
	(loaded goods3 truck3 level0)
	(loaded goods3 truck4 level0)
	(loaded goods3 truck5 level0)
	(loaded goods3 truck6 level0)
	(loaded goods3 truck7 level0)
	(loaded goods3 truck8 level0)
	(loaded goods3 truck9 level0)
	(loaded goods3 truck10 level0)
	(loaded goods4 truck1 level0)
	(loaded goods4 truck2 level0)
	(loaded goods4 truck3 level0)
	(loaded goods4 truck4 level0)
	(loaded goods4 truck5 level0)
	(loaded goods4 truck6 level0)
	(loaded goods4 truck7 level0)
	(loaded goods4 truck8 level0)
	(loaded goods4 truck9 level0)
	(loaded goods4 truck10 level0)
	(loaded goods5 truck1 level0)
	(loaded goods5 truck2 level0)
	(loaded goods5 truck3 level0)
	(loaded goods5 truck4 level0)
	(loaded goods5 truck5 level0)
	(loaded goods5 truck6 level0)
	(loaded goods5 truck7 level0)
	(loaded goods5 truck8 level0)
	(loaded goods5 truck9 level0)
	(loaded goods5 truck10 level0)
	(loaded goods6 truck1 level0)
	(loaded goods6 truck2 level0)
	(loaded goods6 truck3 level0)
	(loaded goods6 truck4 level0)
	(loaded goods6 truck5 level0)
	(loaded goods6 truck6 level0)
	(loaded goods6 truck7 level0)
	(loaded goods6 truck8 level0)
	(loaded goods6 truck9 level0)
	(loaded goods6 truck10 level0)
	(loaded goods7 truck1 level0)
	(loaded goods7 truck2 level0)
	(loaded goods7 truck3 level0)
	(loaded goods7 truck4 level0)
	(loaded goods7 truck5 level0)
	(loaded goods7 truck6 level0)
	(loaded goods7 truck7 level0)
	(loaded goods7 truck8 level0)
	(loaded goods7 truck9 level0)
	(loaded goods7 truck10 level0)
	(loaded goods8 truck1 level0)
	(loaded goods8 truck2 level0)
	(loaded goods8 truck3 level0)
	(loaded goods8 truck4 level0)
	(loaded goods8 truck5 level0)
	(loaded goods8 truck6 level0)
	(loaded goods8 truck7 level0)
	(loaded goods8 truck8 level0)
	(loaded goods8 truck9 level0)
	(loaded goods8 truck10 level0)
	(loaded goods9 truck1 level0)
	(loaded goods9 truck2 level0)
	(loaded goods9 truck3 level0)
	(loaded goods9 truck4 level0)
	(loaded goods9 truck5 level0)
	(loaded goods9 truck6 level0)
	(loaded goods9 truck7 level0)
	(loaded goods9 truck8 level0)
	(loaded goods9 truck9 level0)
	(loaded goods9 truck10 level0)
	(loaded goods10 truck1 level0)
	(loaded goods10 truck2 level0)
	(loaded goods10 truck3 level0)
	(loaded goods10 truck4 level0)
	(loaded goods10 truck5 level0)
	(loaded goods10 truck6 level0)
	(loaded goods10 truck7 level0)
	(loaded goods10 truck8 level0)
	(loaded goods10 truck9 level0)
	(loaded goods10 truck10 level0)
	(loaded goods11 truck1 level0)
	(loaded goods11 truck2 level0)
	(loaded goods11 truck3 level0)
	(loaded goods11 truck4 level0)
	(loaded goods11 truck5 level0)
	(loaded goods11 truck6 level0)
	(loaded goods11 truck7 level0)
	(loaded goods11 truck8 level0)
	(loaded goods11 truck9 level0)
	(loaded goods11 truck10 level0)
	(loaded goods12 truck1 level0)
	(loaded goods12 truck2 level0)
	(loaded goods12 truck3 level0)
	(loaded goods12 truck4 level0)
	(loaded goods12 truck5 level0)
	(loaded goods12 truck6 level0)
	(loaded goods12 truck7 level0)
	(loaded goods12 truck8 level0)
	(loaded goods12 truck9 level0)
	(loaded goods12 truck10 level0)
	(loaded goods13 truck1 level0)
	(loaded goods13 truck2 level0)
	(loaded goods13 truck3 level0)
	(loaded goods13 truck4 level0)
	(loaded goods13 truck5 level0)
	(loaded goods13 truck6 level0)
	(loaded goods13 truck7 level0)
	(loaded goods13 truck8 level0)
	(loaded goods13 truck9 level0)
	(loaded goods13 truck10 level0)
	(loaded goods14 truck1 level0)
	(loaded goods14 truck2 level0)
	(loaded goods14 truck3 level0)
	(loaded goods14 truck4 level0)
	(loaded goods14 truck5 level0)
	(loaded goods14 truck6 level0)
	(loaded goods14 truck7 level0)
	(loaded goods14 truck8 level0)
	(loaded goods14 truck9 level0)
	(loaded goods14 truck10 level0)
	(loaded goods15 truck1 level0)
	(loaded goods15 truck2 level0)
	(loaded goods15 truck3 level0)
	(loaded goods15 truck4 level0)
	(loaded goods15 truck5 level0)
	(loaded goods15 truck6 level0)
	(loaded goods15 truck7 level0)
	(loaded goods15 truck8 level0)
	(loaded goods15 truck9 level0)
	(loaded goods15 truck10 level0)
	(loaded goods16 truck1 level0)
	(loaded goods16 truck2 level0)
	(loaded goods16 truck3 level0)
	(loaded goods16 truck4 level0)
	(loaded goods16 truck5 level0)
	(loaded goods16 truck6 level0)
	(loaded goods16 truck7 level0)
	(loaded goods16 truck8 level0)
	(loaded goods16 truck9 level0)
	(loaded goods16 truck10 level0)
	(loaded goods17 truck1 level0)
	(loaded goods17 truck2 level0)
	(loaded goods17 truck3 level0)
	(loaded goods17 truck4 level0)
	(loaded goods17 truck5 level0)
	(loaded goods17 truck6 level0)
	(loaded goods17 truck7 level0)
	(loaded goods17 truck8 level0)
	(loaded goods17 truck9 level0)
	(loaded goods17 truck10 level0)
	(loaded goods18 truck1 level0)
	(loaded goods18 truck2 level0)
	(loaded goods18 truck3 level0)
	(loaded goods18 truck4 level0)
	(loaded goods18 truck5 level0)
	(loaded goods18 truck6 level0)
	(loaded goods18 truck7 level0)
	(loaded goods18 truck8 level0)
	(loaded goods18 truck9 level0)
	(loaded goods18 truck10 level0)
	(loaded goods19 truck1 level0)
	(loaded goods19 truck2 level0)
	(loaded goods19 truck3 level0)
	(loaded goods19 truck4 level0)
	(loaded goods19 truck5 level0)
	(loaded goods19 truck6 level0)
	(loaded goods19 truck7 level0)
	(loaded goods19 truck8 level0)
	(loaded goods19 truck9 level0)
	(loaded goods19 truck10 level0)
	(loaded goods20 truck1 level0)
	(loaded goods20 truck2 level0)
	(loaded goods20 truck3 level0)
	(loaded goods20 truck4 level0)
	(loaded goods20 truck5 level0)
	(loaded goods20 truck6 level0)
	(loaded goods20 truck7 level0)
	(loaded goods20 truck8 level0)
	(loaded goods20 truck9 level0)
	(loaded goods20 truck10 level0)
	(connected market1 market2)
	(connected market1 market3)
	(connected market1 market4)
	(connected market1 market5)
	(connected market1 market7)
	(connected market1 market8)
	(connected market1 market10)
	(connected market1 market11)
	(connected market1 market12)
	(connected market1 market14)
	(connected market1 market15)
	(connected market2 market1)
	(connected market2 market3)
	(connected market2 market4)
	(connected market2 market5)
	(connected market2 market6)
	(connected market2 market7)
	(connected market2 market8)
	(connected market2 market10)
	(connected market2 market11)
	(connected market2 market12)
	(connected market2 market13)
	(connected market2 market14)
	(connected market2 market15)
	(connected market3 market1)
	(connected market3 market2)
	(connected market3 market4)
	(connected market3 market5)
	(connected market3 market6)
	(connected market3 market7)
	(connected market3 market8)
	(connected market3 market9)
	(connected market3 market10)
	(connected market3 market11)
	(connected market3 market12)
	(connected market3 market13)
	(connected market3 market14)
	(connected market3 market15)
	(connected market4 market1)
	(connected market4 market2)
	(connected market4 market3)
	(connected market4 market5)
	(connected market4 market6)
	(connected market4 market7)
	(connected market4 market8)
	(connected market4 market9)
	(connected market4 market10)
	(connected market4 market11)
	(connected market4 market12)
	(connected market4 market13)
	(connected market4 market14)
	(connected market4 market15)
	(connected market5 market1)
	(connected market5 market2)
	(connected market5 market3)
	(connected market5 market4)
	(connected market5 market6)
	(connected market5 market7)
	(connected market5 market8)
	(connected market5 market9)
	(connected market5 market10)
	(connected market5 market11)
	(connected market5 market12)
	(connected market5 market13)
	(connected market5 market14)
	(connected market6 market2)
	(connected market6 market3)
	(connected market6 market4)
	(connected market6 market5)
	(connected market6 market7)
	(connected market6 market8)
	(connected market6 market9)
	(connected market6 market11)
	(connected market6 market12)
	(connected market6 market14)
	(connected market6 market15)
	(connected market7 market1)
	(connected market7 market2)
	(connected market7 market3)
	(connected market7 market4)
	(connected market7 market5)
	(connected market7 market6)
	(connected market7 market9)
	(connected market7 market10)
	(connected market7 market11)
	(connected market7 market13)
	(connected market7 market14)
	(connected market7 market15)
	(connected market8 market1)
	(connected market8 market2)
	(connected market8 market3)
	(connected market8 market4)
	(connected market8 market5)
	(connected market8 market6)
	(connected market8 market10)
	(connected market8 market11)
	(connected market8 market12)
	(connected market8 market13)
	(connected market8 market14)
	(connected market9 market3)
	(connected market9 market4)
	(connected market9 market5)
	(connected market9 market6)
	(connected market9 market7)
	(connected market9 market10)
	(connected market9 market11)
	(connected market9 market12)
	(connected market9 market13)
	(connected market9 market14)
	(connected market9 market15)
	(connected market10 market1)
	(connected market10 market2)
	(connected market10 market3)
	(connected market10 market4)
	(connected market10 market5)
	(connected market10 market7)
	(connected market10 market8)
	(connected market10 market9)
	(connected market10 market11)
	(connected market10 market12)
	(connected market10 market13)
	(connected market10 market14)
	(connected market11 market1)
	(connected market11 market2)
	(connected market11 market3)
	(connected market11 market4)
	(connected market11 market5)
	(connected market11 market6)
	(connected market11 market7)
	(connected market11 market8)
	(connected market11 market9)
	(connected market11 market10)
	(connected market11 market12)
	(connected market11 market13)
	(connected market11 market14)
	(connected market12 market1)
	(connected market12 market2)
	(connected market12 market3)
	(connected market12 market4)
	(connected market12 market5)
	(connected market12 market6)
	(connected market12 market8)
	(connected market12 market9)
	(connected market12 market10)
	(connected market12 market11)
	(connected market12 market13)
	(connected market12 market14)
	(connected market12 market15)
	(connected market13 market2)
	(connected market13 market3)
	(connected market13 market4)
	(connected market13 market5)
	(connected market13 market7)
	(connected market13 market8)
	(connected market13 market9)
	(connected market13 market10)
	(connected market13 market11)
	(connected market13 market12)
	(connected market13 market14)
	(connected market13 market15)
	(connected market14 market1)
	(connected market14 market2)
	(connected market14 market3)
	(connected market14 market4)
	(connected market14 market5)
	(connected market14 market6)
	(connected market14 market7)
	(connected market14 market8)
	(connected market14 market9)
	(connected market14 market10)
	(connected market14 market11)
	(connected market14 market12)
	(connected market14 market13)
	(connected market14 market15)
	(connected market15 market1)
	(connected market15 market2)
	(connected market15 market3)
	(connected market15 market4)
	(connected market15 market6)
	(connected market15 market7)
	(connected market15 market9)
	(connected market15 market12)
	(connected market15 market13)
	(connected market15 market14)
	(connected depot1 market6)
	(connected market6 depot1)
	(connected depot2 market12)
	(connected market12 depot2)
	(connected depot3 market7)
	(connected market7 depot3)
	(connected depot4 market10)
	(connected market10 depot4)
	(connected depot5 market8)
	(connected market8 depot5)
	(connected depot6 market14)
	(connected market14 depot6)
	(connected depot7 market5)
	(connected market5 depot7)
	(connected depot8 market1)
	(connected market1 depot8)
	(connected depot9 market7)
	(connected market7 depot9)
	(connected depot10 market9)
	(connected market9 depot10)
	(connected depot11 market14)
	(connected market14 depot11)
	(connected depot12 market3)
	(connected market3 depot12)
	(connected depot13 market1)
	(connected market1 depot13)
	(connected depot14 market3)
	(connected market3 depot14)
	(connected depot15 market7)
	(connected market7 depot15)
	(on-sale goods1 market1 level2)
	(on-sale goods2 market1 level2)
	(on-sale goods3 market1 level0)
	(on-sale goods4 market1 level2)
	(on-sale goods5 market1 level2)
	(on-sale goods6 market1 level2)
	(on-sale goods7 market1 level1)
	(on-sale goods8 market1 level0)
	(on-sale goods9 market1 level0)
	(on-sale goods10 market1 level0)
	(on-sale goods11 market1 level2)
	(on-sale goods12 market1 level0)
	(on-sale goods13 market1 level2)
	(on-sale goods14 market1 level0)
	(on-sale goods15 market1 level1)
	(on-sale goods16 market1 level3)
	(on-sale goods17 market1 level2)
	(on-sale goods18 market1 level0)
	(on-sale goods19 market1 level0)
	(on-sale goods20 market1 level1)
	(on-sale goods1 market2 level2)
	(on-sale goods2 market2 level0)
	(on-sale goods3 market2 level2)
	(on-sale goods4 market2 level3)
	(on-sale goods5 market2 level1)
	(on-sale goods6 market2 level3)
	(on-sale goods7 market2 level0)
	(on-sale goods8 market2 level2)
	(on-sale goods9 market2 level0)
	(on-sale goods10 market2 level0)
	(on-sale goods11 market2 level1)
	(on-sale goods12 market2 level2)
	(on-sale goods13 market2 level3)
	(on-sale goods14 market2 level1)
	(on-sale goods15 market2 level0)
	(on-sale goods16 market2 level0)
	(on-sale goods17 market2 level0)
	(on-sale goods18 market2 level0)
	(on-sale goods19 market2 level2)
	(on-sale goods20 market2 level0)
	(on-sale goods1 market3 level0)
	(on-sale goods2 market3 level2)
	(on-sale goods3 market3 level0)
	(on-sale goods4 market3 level0)
	(on-sale goods5 market3 level0)
	(on-sale goods6 market3 level2)
	(on-sale goods7 market3 level2)
	(on-sale goods8 market3 level1)
	(on-sale goods9 market3 level3)
	(on-sale goods10 market3 level1)
	(on-sale goods11 market3 level1)
	(on-sale goods12 market3 level0)
	(on-sale goods13 market3 level3)
	(on-sale goods14 market3 level0)
	(on-sale goods15 market3 level0)
	(on-sale goods16 market3 level2)
	(on-sale goods17 market3 level3)
	(on-sale goods18 market3 level0)
	(on-sale goods19 market3 level0)
	(on-sale goods20 market3 level0)
	(on-sale goods1 market4 level1)
	(on-sale goods2 market4 level3)
	(on-sale goods3 market4 level0)
	(on-sale goods4 market4 level0)
	(on-sale goods5 market4 level0)
	(on-sale goods6 market4 level0)
	(on-sale goods7 market4 level0)
	(on-sale goods8 market4 level0)
	(on-sale goods9 market4 level2)
	(on-sale goods10 market4 level0)
	(on-sale goods11 market4 level3)
	(on-sale goods12 market4 level1)
	(on-sale goods13 market4 level0)
	(on-sale goods14 market4 level2)
	(on-sale goods15 market4 level1)
	(on-sale goods16 market4 level2)
	(on-sale goods17 market4 level3)
	(on-sale goods18 market4 level0)
	(on-sale goods19 market4 level0)
	(on-sale goods20 market4 level0)
	(on-sale goods1 market5 level0)
	(on-sale goods2 market5 level0)
	(on-sale goods3 market5 level0)
	(on-sale goods4 market5 level0)
	(on-sale goods5 market5 level3)
	(on-sale goods6 market5 level1)
	(on-sale goods7 market5 level0)
	(on-sale goods8 market5 level0)
	(on-sale goods9 market5 level2)
	(on-sale goods10 market5 level0)
	(on-sale goods11 market5 level3)
	(on-sale goods12 market5 level3)
	(on-sale goods13 market5 level0)
	(on-sale goods14 market5 level2)
	(on-sale goods15 market5 level0)
	(on-sale goods16 market5 level0)
	(on-sale goods17 market5 level0)
	(on-sale goods18 market5 level0)
	(on-sale goods19 market5 level0)
	(on-sale goods20 market5 level0)
	(on-sale goods1 market6 level0)
	(on-sale goods2 market6 level0)
	(on-sale goods3 market6 level0)
	(on-sale goods4 market6 level0)
	(on-sale goods5 market6 level3)
	(on-sale goods6 market6 level1)
	(on-sale goods7 market6 level0)
	(on-sale goods8 market6 level3)
	(on-sale goods9 market6 level1)
	(on-sale goods10 market6 level2)
	(on-sale goods11 market6 level3)
	(on-sale goods12 market6 level2)
	(on-sale goods13 market6 level0)
	(on-sale goods14 market6 level0)
	(on-sale goods15 market6 level1)
	(on-sale goods16 market6 level3)
	(on-sale goods17 market6 level0)
	(on-sale goods18 market6 level2)
	(on-sale goods19 market6 level0)
	(on-sale goods20 market6 level0)
	(on-sale goods1 market7 level1)
	(on-sale goods2 market7 level1)
	(on-sale goods3 market7 level0)
	(on-sale goods4 market7 level3)
	(on-sale goods5 market7 level0)
	(on-sale goods6 market7 level1)
	(on-sale goods7 market7 level0)
	(on-sale goods8 market7 level3)
	(on-sale goods9 market7 level0)
	(on-sale goods10 market7 level3)
	(on-sale goods11 market7 level1)
	(on-sale goods12 market7 level0)
	(on-sale goods13 market7 level2)
	(on-sale goods14 market7 level0)
	(on-sale goods15 market7 level2)
	(on-sale goods16 market7 level0)
	(on-sale goods17 market7 level0)
	(on-sale goods18 market7 level3)
	(on-sale goods19 market7 level1)
	(on-sale goods20 market7 level0)
	(on-sale goods1 market8 level0)
	(on-sale goods2 market8 level1)
	(on-sale goods3 market8 level2)
	(on-sale goods4 market8 level0)
	(on-sale goods5 market8 level0)
	(on-sale goods6 market8 level3)
	(on-sale goods7 market8 level0)
	(on-sale goods8 market8 level0)
	(on-sale goods9 market8 level2)
	(on-sale goods10 market8 level1)
	(on-sale goods11 market8 level1)
	(on-sale goods12 market8 level0)
	(on-sale goods13 market8 level0)
	(on-sale goods14 market8 level1)
	(on-sale goods15 market8 level3)
	(on-sale goods16 market8 level2)
	(on-sale goods17 market8 level0)
	(on-sale goods18 market8 level3)
	(on-sale goods19 market8 level0)
	(on-sale goods20 market8 level0)
	(on-sale goods1 market9 level2)
	(on-sale goods2 market9 level2)
	(on-sale goods3 market9 level0)
	(on-sale goods4 market9 level2)
	(on-sale goods5 market9 level0)
	(on-sale goods6 market9 level2)
	(on-sale goods7 market9 level2)
	(on-sale goods8 market9 level0)
	(on-sale goods9 market9 level0)
	(on-sale goods10 market9 level0)
	(on-sale goods11 market9 level0)
	(on-sale goods12 market9 level3)
	(on-sale goods13 market9 level1)
	(on-sale goods14 market9 level1)
	(on-sale goods15 market9 level3)
	(on-sale goods16 market9 level0)
	(on-sale goods17 market9 level0)
	(on-sale goods18 market9 level2)
	(on-sale goods19 market9 level1)
	(on-sale goods20 market9 level0)
	(on-sale goods1 market10 level1)
	(on-sale goods2 market10 level3)
	(on-sale goods3 market10 level1)
	(on-sale goods4 market10 level3)
	(on-sale goods5 market10 level1)
	(on-sale goods6 market10 level3)
	(on-sale goods7 market10 level3)
	(on-sale goods8 market10 level2)
	(on-sale goods9 market10 level3)
	(on-sale goods10 market10 level1)
	(on-sale goods11 market10 level3)
	(on-sale goods12 market10 level3)
	(on-sale goods13 market10 level3)
	(on-sale goods14 market10 level3)
	(on-sale goods15 market10 level3)
	(on-sale goods16 market10 level2)
	(on-sale goods17 market10 level3)
	(on-sale goods18 market10 level2)
	(on-sale goods19 market10 level3)
	(on-sale goods20 market10 level2)
	(on-sale goods1 market11 level3)
	(on-sale goods2 market11 level1)
	(on-sale goods3 market11 level2)
	(on-sale goods4 market11 level2)
	(on-sale goods5 market11 level3)
	(on-sale goods6 market11 level2)
	(on-sale goods7 market11 level2)
	(on-sale goods8 market11 level3)
	(on-sale goods9 market11 level2)
	(on-sale goods10 market11 level0)
	(on-sale goods11 market11 level2)
	(on-sale goods12 market11 level0)
	(on-sale goods13 market11 level0)
	(on-sale goods14 market11 level3)
	(on-sale goods15 market11 level2)
	(on-sale goods16 market11 level2)
	(on-sale goods17 market11 level1)
	(on-sale goods18 market11 level0)
	(on-sale goods19 market11 level1)
	(on-sale goods20 market11 level0)
	(on-sale goods1 market12 level0)
	(on-sale goods2 market12 level3)
	(on-sale goods3 market12 level0)
	(on-sale goods4 market12 level2)
	(on-sale goods5 market12 level0)
	(on-sale goods6 market12 level0)
	(on-sale goods7 market12 level1)
	(on-sale goods8 market12 level2)
	(on-sale goods9 market12 level0)
	(on-sale goods10 market12 level0)
	(on-sale goods11 market12 level0)
	(on-sale goods12 market12 level3)
	(on-sale goods13 market12 level2)
	(on-sale goods14 market12 level0)
	(on-sale goods15 market12 level2)
	(on-sale goods16 market12 level3)
	(on-sale goods17 market12 level3)
	(on-sale goods18 market12 level2)
	(on-sale goods19 market12 level0)
	(on-sale goods20 market12 level0)
	(on-sale goods1 market13 level0)
	(on-sale goods2 market13 level2)
	(on-sale goods3 market13 level2)
	(on-sale goods4 market13 level1)
	(on-sale goods5 market13 level0)
	(on-sale goods6 market13 level0)
	(on-sale goods7 market13 level3)
	(on-sale goods8 market13 level1)
	(on-sale goods9 market13 level3)
	(on-sale goods10 market13 level1)
	(on-sale goods11 market13 level0)
	(on-sale goods12 market13 level0)
	(on-sale goods13 market13 level0)
	(on-sale goods14 market13 level3)
	(on-sale goods15 market13 level0)
	(on-sale goods16 market13 level1)
	(on-sale goods17 market13 level0)
	(on-sale goods18 market13 level0)
	(on-sale goods19 market13 level0)
	(on-sale goods20 market13 level2)
	(on-sale goods1 market14 level0)
	(on-sale goods2 market14 level0)
	(on-sale goods3 market14 level0)
	(on-sale goods4 market14 level1)
	(on-sale goods5 market14 level3)
	(on-sale goods6 market14 level0)
	(on-sale goods7 market14 level0)
	(on-sale goods8 market14 level1)
	(on-sale goods9 market14 level1)
	(on-sale goods10 market14 level0)
	(on-sale goods11 market14 level0)
	(on-sale goods12 market14 level0)
	(on-sale goods13 market14 level3)
	(on-sale goods14 market14 level1)
	(on-sale goods15 market14 level2)
	(on-sale goods16 market14 level0)
	(on-sale goods17 market14 level0)
	(on-sale goods18 market14 level2)
	(on-sale goods19 market14 level1)
	(on-sale goods20 market14 level1)
	(on-sale goods1 market15 level0)
	(on-sale goods2 market15 level0)
	(on-sale goods3 market15 level3)
	(on-sale goods4 market15 level1)
	(on-sale goods5 market15 level1)
	(on-sale goods6 market15 level0)
	(on-sale goods7 market15 level0)
	(on-sale goods8 market15 level0)
	(on-sale goods9 market15 level1)
	(on-sale goods10 market15 level0)
	(on-sale goods11 market15 level0)
	(on-sale goods12 market15 level2)
	(on-sale goods13 market15 level0)
	(on-sale goods14 market15 level0)
	(on-sale goods15 market15 level0)
	(on-sale goods16 market15 level0)
	(on-sale goods17 market15 level1)
	(on-sale goods18 market15 level0)
	(on-sale goods19 market15 level2)
	(on-sale goods20 market15 level2)
	(at truck1 depot14)
	(at truck2 depot1)
	(at truck3 depot14)
	(at truck4 depot1)
	(at truck5 depot3)
	(at truck6 depot5)
	(at truck7 depot6)
	(at truck8 depot13)
	(at truck9 depot6)
	(at truck10 depot13))

(:goal (and
	(stored goods1 level11)
	(stored goods2 level20)
	(stored goods3 level9)
	(stored goods4 level17)
	(stored goods5 level12)
	(stored goods6 level3)
	(stored goods7 level4)
	(stored goods8 level1)
	(stored goods9 level11)
	(stored goods10 level2)
	(stored goods11 level16)
	(stored goods12 level2)
	(stored goods13 level11)
	(stored goods14 level8)
	(stored goods15 level14)
	(stored goods16 level16)
	(stored goods17 level6)
	(stored goods18 level14)
	(stored goods19 level7)
	(stored goods20 level2)))

)
