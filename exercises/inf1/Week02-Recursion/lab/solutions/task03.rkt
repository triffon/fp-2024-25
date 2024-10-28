#lang racket

(define (road-to-logprog stairs-count)
  (cond
    [(= stairs-count 1) 1]
    [(= stairs-count 2) 2]
    [else (+ (road-to-logprog (sub1 stairs-count)) (road-to-logprog (- stairs-count 2)))]
    )
  )

(= (road-to-logprog 4) 5)
(= (road-to-logprog 5) 8)
(= (road-to-logprog 10) 89)