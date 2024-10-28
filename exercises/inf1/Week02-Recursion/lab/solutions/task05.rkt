#lang racket

(define (my-recursive-pow x n)
  (cond
    [(zero? n) 1]
    [else (* x (my-recursive-pow x (sub1 n)))]
    )
  )

(= (my-recursive-pow 2 5) 32)
(= (my-recursive-pow 15 3) 3375)