#lang racket

(define (num-to-xs num)
  (define (helper current result)
    (cond
      [(zero? current) result]
      [else (helper (quotient current 10) (cons (remainder current 10) result))]
      )
    )
   (if (zero? num) '(0) (helper num '()))
  )

(equal? (num-to-xs 123) '(1 2 3))
(equal? (num-to-xs 123456789) '(1 2 3 4 5 6 7 8 9))

(= (xs-to-num '(1 2 3)) 123)
(= (xs-to-num '(1 2 3 4 5 6 7 8 9)) 123456789)