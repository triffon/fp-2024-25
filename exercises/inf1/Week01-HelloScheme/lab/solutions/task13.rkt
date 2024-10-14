#lang racket

(define (mirror? n)
  (define ones (remainder n 10))
  (define tens (remainder (quotient n 10) 10))
  (define hundreds (remainder (quotient n 100) 10))
  (define thousands (quotient n 1000))
  (and (= ones thousands) (= tens hundreds))
  )

(equal? (mirror? 2332) #t)
(equal? (mirror? 1111) #t)
(equal? (mirror? 5005) #t)
(equal? (mirror? 1234) #f)