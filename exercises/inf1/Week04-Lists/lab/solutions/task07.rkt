#lang racket

(define (my-reverse-foldl xs)
  (foldl (λ (x acc) (cons x acc)) '() xs)
  )

(equal? (my-reverse-foldl '(1 2 3 4 5)) '(5 4 3 2 1))