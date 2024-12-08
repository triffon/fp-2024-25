#lang racket

(define (assoc-keys xs)
  (map (λ (pair) (car pair)) xs)
  )

(define (assoc-values xs)
  (map cdr xs)
  )


(equal? (assoc-keys '((1 . "One") (2 . "Two") (3 . "Three"))) '(1 2 3))
(equal? (assoc-keys '()) '())

(equal? (assoc-values '((1 . "One") (2 . "Two") (3 . "Three"))) '("One" "Two" "Three"))
(equal? (assoc-values '()) '())
