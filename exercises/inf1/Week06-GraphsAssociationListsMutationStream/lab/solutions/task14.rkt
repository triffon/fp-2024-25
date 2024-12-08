#lang racket

(define empty-stream '())

(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t) (cons h (delay t))))
  )

(define head car)
(define (tail s)
  (force (cdr s)))

(define (stream-take n stream)
  (if (or (zero? n) (empty? stream)) '() (cons (head stream) (stream-take (sub1 n) (tail stream))))
  )

(define (stream-range a b)
  (if (> a b) empty-stream (cons-stream a (stream-range (add1 a) b)))
  )

(equal? (stream-take 5 (stream-range 5 100)) '(5 6 7 8 9))
(equal? (stream-take 8 (stream-range 1 3)) '(1 2 3))