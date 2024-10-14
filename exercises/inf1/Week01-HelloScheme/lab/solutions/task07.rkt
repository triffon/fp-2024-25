#lang racket

(define (not-equal-one-line? a b) (not (= a b)))

(equal? (not-equal-one-line? 5 2) #t)
(equal? (not-equal-one-line? 5 5) #f)