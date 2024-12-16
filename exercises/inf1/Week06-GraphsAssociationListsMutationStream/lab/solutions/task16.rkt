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

(define (stream-map f stream)
  (if (empty? stream)
      empty-stream
      (cons-stream (f (head stream)) (stream-map f (tail stream)))
      )
  )

(equal? (stream-take 5 (stream-map (λ (x) (* x 2)) natural-numbers)) '(2 4 6 8 10))
(equal? (stream-take 8 (stream-map (λ (x) (* x 2)) natural-numbers)) '(2 4 6 8 10 12 14 16))