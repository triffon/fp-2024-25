#lang racket

(define (make-f-alist xs f)
  (map (λ (x) (cons x (f x))) xs)
  )

(equal? (make-f-alist '(1 2 3) add1) '((1 . 2) (2 . 3) (3 . 4)))