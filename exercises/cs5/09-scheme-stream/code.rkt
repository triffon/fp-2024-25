#lang racket

(define-syntax my-and
  (syntax-rules ()
    ((my-and x y)
     (if x y #f))))

(define-syntax my-delay
  (syntax-rules ()
    ((my-delay x)
     (lambda () x))))

(define (my-force p)
  (p))

(define-syntax my-stream-cons
  (syntax-rules ()
    ((my-stream-cons x strm)
     (cons x (my-delay strm)))))

(define (my-stream-first strm)
  (car strm))

(define (my-stream-rest strm)
  (my-force (cdr strm)))

(define (my-stream-takelist strm n)
  (if (= n 0)
      '()
      (cons (my-stream-first strm) (my-stream-takelist (my-stream-rest strm) (- n 1)))))

(define (my-gen-nats from)
  (my-stream-cons from (my-gen-nats (+ from 1))))

(define (my-gen-nats2 from)
  (cons from (lambda () (my-gen-nats2 (+ from 1)))))

(define my-nats (my-gen-nats 0))


; from here on we use the builtin streams

(define (stream-takelist strm n)
  (stream->list (stream-take strm n)))

(define (gen-nats from)
  (stream-cons from (gen-nats (+ from 1))))

(define nats (gen-nats 0))