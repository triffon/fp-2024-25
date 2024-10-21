#lang racket

(define (rev n)
  (define (helper result current)
    (if (zero? current)
        result
        (helper (+ (* result 10) (remainder current 10)) (quotient current 10))
        )
    )
  (helper 0 n)
  )

(= (rev 0) 0)
(= (rev 123) 321)
(= (rev 987654321) 123456789)
(= (rev 12000) 21)