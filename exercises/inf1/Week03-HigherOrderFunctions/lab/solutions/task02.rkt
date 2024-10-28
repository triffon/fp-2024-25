#lang racket

(define (complex-procedure f g)
  (λ (x y) (g (f x) (f y)))
  )

(= ((complex-procedure (λ (x) (* x 2)) (λ (x y) (+ x y))) 5 8) 26)