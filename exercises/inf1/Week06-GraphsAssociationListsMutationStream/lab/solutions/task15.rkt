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

(define (infinite-stream start)
  (cons-stream start (infinite-stream (add1 start)))
  )

(define natural-numbers (infinite-stream 1))

(equal? (stream-take 5 natural-numbers) '(1 2 3 4 5))
(equal? (stream-take 8 natural-numbers) '(1 2 3 4 5 6 7 8))