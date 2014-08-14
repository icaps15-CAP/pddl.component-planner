; woodworking task with 21 parts and 140% wood
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 176206

(define (problem wood-prob-sat-1)
  (:domain woodworking)
  (:objects
    grinder0 - grinder
    glazer0 - glazer
    immersion-varnisher0 - immersion-varnisher
    planer0 - planer
    highspeed-saw0 - highspeed-saw
    spray-varnisher0 - spray-varnisher
    saw0 - saw
    red green blue mauve black white - acolour
    mahogany oak beech teak cherry - awood
    p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 - part
    b0 b1 b2 b3 b4 b5 b6 b7 b8 - board
    s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 - aboardsize
  )
  (:init
    (grind-treatment-change varnished colourfragments)
    (grind-treatment-change glazed untreated)
    (grind-treatment-change untreated untreated)
    (grind-treatment-change colourfragments untreated)
    (is-smooth smooth)
    (is-smooth verysmooth)
    
    (boardsize-successor s0 s1)
    (boardsize-successor s1 s2)
    (boardsize-successor s2 s3)
    (boardsize-successor s3 s4)
    (boardsize-successor s4 s5)
    (boardsize-successor s5 s6)
    (boardsize-successor s6 s7)
    (boardsize-successor s7 s8)
    (boardsize-successor s8 s9)
    (boardsize-successor s9 s10)
    (boardsize-successor s10 s11)
    (has-colour glazer0 blue)
    (has-colour glazer0 black)
    (has-colour glazer0 white)
    (has-colour glazer0 green)
    (has-colour glazer0 mauve)
    (has-colour immersion-varnisher0 blue)
    (has-colour immersion-varnisher0 mauve)
    (has-colour immersion-varnisher0 white)
    (has-colour immersion-varnisher0 green)
    (has-colour immersion-varnisher0 black)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 blue)
    (has-colour spray-varnisher0 mauve)
    (has-colour spray-varnisher0 white)
    (has-colour spray-varnisher0 green)
    (has-colour spray-varnisher0 black)
    (available p0)
    (colour p0 red)
    (wood p0 teak)
    (surface-condition p0 verysmooth)
    (treatment p0 colourfragments)
    (goalsize p0 medium)
    
    
    
    
    (unused p1)
    (goalsize p1 large)
    
    
    
    
    (unused p2)
    (goalsize p2 medium)
    
    
    
    
    (unused p3)
    (goalsize p3 small)
    
    
    
    
    (unused p4)
    (goalsize p4 medium)
    
    
    
    
    (unused p5)
    (goalsize p5 medium)
    
    
    
    
    (unused p6)
    (goalsize p6 large)
    
    
    
    
    (unused p7)
    (goalsize p7 medium)
    
    
    
    
    (unused p8)
    (goalsize p8 small)
    
    
    
    
    (unused p9)
    (goalsize p9 medium)
    
    
    
    
    (unused p10)
    (goalsize p10 small)
    
    
    
    
    (unused p11)
    (goalsize p11 large)
    
    
    
    
    (unused p12)
    (goalsize p12 medium)
    
    
    
    
    (unused p13)
    (goalsize p13 medium)
    
    
    
    
    (unused p14)
    (goalsize p14 medium)
    
    
    
    
    (unused p15)
    (goalsize p15 small)
    
    
    
    
    (unused p16)
    (goalsize p16 small)
    
    
    
    
    (unused p17)
    (goalsize p17 large)
    
    
    
    
    (unused p18)
    (goalsize p18 large)
    
    
    
    
    (unused p19)
    (goalsize p19 medium)
    
    
    
    
    (unused p20)
    (goalsize p20 large)
    
    
    
    
    (boardsize b0 s11)
    (wood b0 beech)
    (surface-condition b0 rough)
    (available b0)
    (boardsize b1 s8)
    (wood b1 beech)
    (surface-condition b1 smooth)
    (available b1)
    (boardsize b2 s2)
    (wood b2 beech)
    (surface-condition b2 rough)
    (available b2)
    (boardsize b3 s7)
    (wood b3 teak)
    (surface-condition b3 smooth)
    (available b3)
    (boardsize b4 s5)
    (wood b4 teak)
    (surface-condition b4 smooth)
    (available b4)
    (boardsize b5 s9)
    (wood b5 mahogany)
    (surface-condition b5 rough)
    (available b5)
    (boardsize b6 s7)
    (wood b6 oak)
    (surface-condition b6 smooth)
    (available b6)
    (boardsize b7 s5)
    (wood b7 oak)
    (surface-condition b7 rough)
    (available b7)
    (boardsize b8 s6)
    (wood b8 cherry)
    (surface-condition b8 rough)
    (available b8)
  )
  (:goal
    (and
      (available p0)
      (colour p0 green)
      (wood p0 teak)
      (available p1)
      (colour p1 black)
      (treatment p1 glazed)
      (available p2)
      (colour p2 green)
      (treatment p2 glazed)
      (available p3)
      (wood p3 oak)
      (treatment p3 glazed)
      (available p4)
      (colour p4 black)
      (wood p4 beech)
      (surface-condition p4 smooth)
      (treatment p4 glazed)
      (available p5)
      (surface-condition p5 smooth)
      (treatment p5 glazed)
      (available p6)
      (colour p6 white)
      (wood p6 beech)
      (surface-condition p6 verysmooth)
      (available p7)
      (wood p7 mahogany)
      (treatment p7 varnished)
      (available p8)
      (colour p8 blue)
      (wood p8 cherry)
      (surface-condition p8 verysmooth)
      (available p9)
      (colour p9 white)
      (wood p9 beech)
      (surface-condition p9 verysmooth)
      (treatment p9 varnished)
      (available p10)
      (colour p10 green)
      (wood p10 oak)
      (available p11)
      (wood p11 beech)
      (treatment p11 glazed)
      (available p12)
      (colour p12 green)
      (wood p12 beech)
      (available p13)
      (colour p13 blue)
      (wood p13 oak)
      (surface-condition p13 verysmooth)
      (treatment p13 varnished)
      (available p14)
      (colour p14 mauve)
      (wood p14 oak)
      (surface-condition p14 verysmooth)
      (available p15)
      (colour p15 mauve)
      (treatment p15 glazed)
      (available p16)
      (colour p16 mauve)
      (wood p16 teak)
      (surface-condition p16 smooth)
      (treatment p16 varnished)
      (available p17)
      (colour p17 black)
      (wood p17 beech)
      (available p18)
      (colour p18 black)
      (wood p18 mahogany)
      (surface-condition p18 verysmooth)
      (treatment p18 glazed)
      (available p19)
      (surface-condition p19 verysmooth)
      (treatment p19 varnished)
      (available p20)
      (surface-condition p20 verysmooth)
      (treatment p20 glazed)
    )
  )
  
)
