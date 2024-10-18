#lang racket

;; 1
(define (factorial n)
  (if (<= n 0)
      1
      (* n (factorial (- n 1)))))

;; 2.
; indexing from 0
(define (fibonacci n)
  (cond
    ((= 0 n) 0)
    ((= 1 n) 1)
    (else (+
           (fibonacci (- n 1))
           (fibonacci (- n 2))))))

;; 3.
(define (sum-interval a b)
  (if (> a b)
      0
      (+ a (sum-interval (+ 1 a) b))))

;; 4.
(define (count-digits number)
  (if (< -10 number 10)
      1
      (+ 1 (count-digits (quotient number 10)))))

;; 5.
(define (reverse-digits number)
  (define (helper n acc)
    (if (= 0 n)
        acc
        (helper (quotient n 10) (+ (* 10 acc) (remainder n 10)))))

  (if (< 0 number)
      (- (helper (- number) 0))
      (helper number 0)))

;; 6.
(define (sum-list lst)
  (if (empty? lst)
      0
      (+ (car lst) (sum-list (cdr lst)))))

