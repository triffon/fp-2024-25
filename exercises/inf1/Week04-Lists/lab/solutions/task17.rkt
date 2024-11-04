#lang racket

(define (my-cartesian-product xs ys)
  (define (helper x)
    (map (λ (y) (cons x y)) ys)
    )
  (foldl (λ (x acc) (append acc (helper x))) '() xs)
  )

(equal? (my-cartesian-product '(1 2) '(3 4)) '((1 . 3) (1 . 4) (2 . 3) (2 . 4)))
(equal? (my-cartesian-product '(1 2 3 4 5) '(6 7 8)) '((1 . 6) (1 . 7) (1 . 8) (2 . 6) (2 . 7) (2 . 8) (3 . 6) (3 . 7) (3 . 8) (4 . 6) (4 . 7) (4 . 8) (5 . 6) (5 . 7) (5 . 8)))