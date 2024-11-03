#lang racket

(define (tabulate f)
  (λ (a b) (map (λ (x) (cons x (f x))) (range a (add1 b))))
  )

(equal? ((tabulate (λ (x) (* x x))) 1 5) '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)))