#lang racket

(define-syntax my-delay
  (syntax-rules ()
    ((my-delay x)
     (lambda () x))))

(define (my-force lazy)
  (lazy))

(define-syntax my-stream-cons
  (syntax-rules ()
    ((my-stream-cons el strm)
     (cons
      el
      (my-delay strm)))))

(define (my-stream-first strm)
  (car strm))

(define (my-stream-rest strm)
  (my-force (cdr strm)))

(define (my-stream-takelist strm n)
  (if (= n 0)
      '()
      (cons (my-stream-first strm)
            (my-stream-takelist (my-stream-rest strm) (- n 1)))))



(define (my-gen-nats-dumb from)
  (cons
   from
   (lambda () (my-gen-nats-dumb (+ 1 from)))))

(define (my-gen-nats from)
  (my-stream-cons
   from
   (my-gen-nats (+ 1 from))))

(define my-nats (my-gen-nats 0))

(define (stream-takelist strm n)
  (stream->list (stream-take strm n)))

(define (gen-nats from)
  (stream-cons
   from
   (gen-nats (+ 1 from))))

(define nats (gen-nats 0))