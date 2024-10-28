#lang racket

(define (fib-recursive x)
  (cond
    [(zero? x) 0]
    [(= x 1) 1]
    [else (+ (fib-recursive (sub1 x)) (fib-recursive (- x 2)))]
    )
  )

(define (fib-iterative x)
  (define (helper n0 n1 current)
    (cond
      [(zero? current) n0]
      [(= current 1) n1]
      [else (helper n1 (+ n0 n1) (sub1 current))]
      )
    )
  (helper 0 1 x)
  )

(= (fib-recursive 12) 144)

(= (fib-iterative 12) 144)