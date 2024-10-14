#lang racket

(define (fact-recursive n)
  (if (<= n 1) 1 (* n (fact-recursive (- n 1))))
  )

(fact-recursive 5)
