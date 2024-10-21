#lang racket

(define (partition-function n)
  (define (helper remaining total)
    (cond
      [(zero? remaining) 1]
      [(or (< remaining 0) (= total 0)) 0]
      [else (+ (helper remaining (sub1 total)) (helper (- remaining total) total))]
      )
    )
  (helper n n)
  )

(= (partition-function 4) 5)
(= (partition-function 5) 7)
(= (partition-function 10) 42)