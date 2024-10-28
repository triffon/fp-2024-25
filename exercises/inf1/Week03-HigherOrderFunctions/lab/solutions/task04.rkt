#lang racket

(define (accumulate op nv a b term next)
    (if (> a b) nv
        (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
    (if (> a b) nv
        (accumulate-i op (op nv (term a)) (next a) b term next)))

; 2^3 + 5^3 + 8^3 + ... + n^3

(define (cool-expression upper-bound)
  (accumulate + 0 2 upper-bound (λ (x) (expt x 3)) (λ (x) (+ x 3)))
  )

(= (cool-expression 11) 1976)
(= (cool-expression 15) 4720)