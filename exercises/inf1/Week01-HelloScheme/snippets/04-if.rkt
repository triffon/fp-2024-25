#lang racket

(define (bigger-than-10? n)
  (if (> n 10)
    (displayln "Yes")
    (displayln "No")
  )
)

(bigger-than-10? 15)
(bigger-than-10? 5)

(define (dummy-example n)
  (if (< n 0) (/ n 0) (* 2 n))
)

(dummy-example 5)
; (dummy-example -5)