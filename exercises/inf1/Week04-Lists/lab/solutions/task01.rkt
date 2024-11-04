#lang racket

(define (sort-list xs)
  (λ (pred?) (sort xs pred?))
  )

(equal? ((sort-list '("one" "two" "0" "five" "" "one hundred" "onehundred"))
         (λ (s1 s2) (< (string-length s1) (string-length s2)))) '("" "0" "one" "two" "five" "onehundred" "one hundred"))