#lang racket

(define (zero-rows xss)
  (map (λ (xs) (if (list? (member 0 xs)) (map (λ (x) 0) xs) xs)) xss)
  )

(equal? (zero-rows '((1 2 0) (3 4 1) (0 5 7) (4 2 4))) '((0 0 0) (3 4 1) (0 0 0) (4 2 4)))