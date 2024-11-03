#lang racket

(define (concat xs ys)
  (if (empty? xs)
      ys
      (cons (car xs) (concat (cdr xs) ys))
      )
  )

(equal? (concat '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))