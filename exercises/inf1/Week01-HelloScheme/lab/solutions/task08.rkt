#lang racket

(define (between? a b n)
  (or (and (> a n) (< b n))
      (and (< a n) (> b n))
      )
  )

(equal? (between? 1 5 4) #t)
(equal? (between? 5 1 4) #t)
(equal? (between? 10 50 200) #f)
(equal? (between? 10 50 1) #f)