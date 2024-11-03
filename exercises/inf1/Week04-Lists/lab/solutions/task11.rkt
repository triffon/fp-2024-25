#lang racket

(define (divides? x y)
  (zero? (remainder y x))
  )

(define (factorise n)
  (define (helper left-over result current-divisor)
    (cond
      [(= left-over 1) result]
      [(divides? current-divisor left-over) (helper (quotient left-over current-divisor) (cons current-divisor result) current-divisor)]
      [else (helper left-over result (add1 current-divisor))]
      )
    )
  (reverse (helper n '() 2))
)

(equal? (factorise 2) '(2))
(equal? (factorise 6) '(2 3))
(equal? (factorise 13) '(13))
(equal? (factorise 123) '(3 41))
(equal? (factorise 152) '(2 2 2 19))
(equal? (factorise 12356498) '(2 7 11 19 41 103))