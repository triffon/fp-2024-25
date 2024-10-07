#lang racket

(define (guards-example n)
  (cond
    [(= n 100) (displayln "n is 100")]
    [(zero? (remainder n 2)) (displayln "n is not 100 but is even")]
    [else (displayln "n is not 100 and is not even")]
  )
)

(guards-example 100)
(guards-example 66)
(guards-example 5)