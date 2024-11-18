#lang racket

(define counter 0)

(define (increment-counter)
  (set! counter (+ counter 1)))

(define (get-counter) counter)

(displayln (get-counter))
(increment-counter)
(displayln (get-counter))

(increment-counter)
(increment-counter)
(increment-counter)
(increment-counter)

(displayln (get-counter))
