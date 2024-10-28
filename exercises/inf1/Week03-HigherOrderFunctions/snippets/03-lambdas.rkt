#lang racket

(define (calculate a b op)
  (op a b)
  )

(calculate 5 3 (λ (a b) (+ a a a b b)))

(λ (x) (+ x 5))
(λ (x) (< x 10))
(λ (a b c) (+ a b c))

((λ (x y z) (string-append x y z)) "This" " is " "Racket!")
((λ (x y z) (x y z)) (λ (x y) (+ x y)) 5 6)