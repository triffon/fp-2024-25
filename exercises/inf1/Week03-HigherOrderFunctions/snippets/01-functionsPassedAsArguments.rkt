#lang racket

(define (my-complex-binary-procedure a b)
  (expt (+ a a b) 3)
  )

(define (calculate a b op)
  (op a b)
  )

(calculate 5 3 +)
(calculate 5 3 -)
(calculate 5 3 *)
(calculate 5 3 /)
(calculate 5 3 quotient)
(calculate 5 3 remainder)
(calculate 5 3 min)
(calculate 5 3 max)
(calculate 5 3 my-complex-binary-procedure)