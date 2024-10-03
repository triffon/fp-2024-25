#lang racket

(define foo 50)

(define (recip x)
  (if (= x 0)
      #f
      (/ 1 x)))

(define (my-not x)
  (if x
      #f
      #t))

(define (my-and x y)
  (if x
      y
      #f))

(define (count-digits x) (count-digits-i x 0))

(define (count-digits-i x num-digits-so-far)
  (if (= x 0)
      num-digits-so-far
      (count-digits-i (quotient x 10) (+ 1 num-digits-so-far))))

(define (reverse x base)
  (reverse-into 0 x base))

(define (reverse-into reversed x base)
  (if (= x 0)
      reversed
      (reverse-into
       (+ (* reversed base) (remainder x base))
       (quotient x base)
       base)))

(define (palindrome? x base)
  (= x (reverse x base)))