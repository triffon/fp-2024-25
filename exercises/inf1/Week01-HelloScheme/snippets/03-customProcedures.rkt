#lang racket

(define (my-first-procedure) 
    (displayln "This is my first procedure in Scheme")
)
(my-first-procedure)

(define (calculate-rectangle-area a b) (* a b))
(calculate-rectangle-area 5 6)

(define (some-complex-expression a b c) 
    (* (+ (quotient b a) c) (- a b))
)
(some-complex-expression 3 4 5)
