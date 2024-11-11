#lang racket

(define (apply-and-sum xs fs)
  (filter odd? (map (λ (x) (foldl + 0 (map (λ (f) (f x)) fs))) xs))
  )

(equal? (apply-and-sum '(1 2 3) (list (λ (x) (* x x)) (λ (x) (+ x 3)))) '(5 9 15))
(equal? (apply-and-sum '(4 5 6) (list (λ (x) (* x 2)) (λ (x) (- x 1)))) '(11 17))
(equal? (apply-and-sum '(-2 0 3) (list (λ (x) (* x x)) (λ (x) (+ x 2)))) '())