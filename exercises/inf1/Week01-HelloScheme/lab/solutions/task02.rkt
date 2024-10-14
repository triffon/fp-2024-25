#lang racket

(define (three-digit-number? n)
  (or (and (> n 99) (< n 1000))
      (and (< n -99) (> n -1000))
      )
  )

(equal? (three-digit-number? 125) #t)
(equal? (three-digit-number? -125) #t)
(equal? (three-digit-number? 56) #f)
(equal? (three-digit-number? 3456) #f)