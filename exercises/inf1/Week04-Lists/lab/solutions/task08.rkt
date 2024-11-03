#lang racket

(define (kth-max-negative xs)
  (λ (k) (list-ref (sort (remove-duplicates (filter negative? xs)) >) (sub1 k)))
  )

(= ((kth-max-negative '(-1)) 1) -1)
(= ((kth-max-negative '(-1 -5 -6 -6 -6 -6)) 2) -5)
(= ((kth-max-negative '(1 2 3 4 -5 6 7 -2 -1 0)) 2) -2)