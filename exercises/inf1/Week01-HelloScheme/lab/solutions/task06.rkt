#lang racket

(define (my-max-built-in a b) (max a b))

(define (my-max-if a b) (if (< a b) b a))

(define (my-max-guard a b)
  (cond
   [(> a b) a]
   [else b]
   )
  )

(= (my-max-built-in 5 6) 6)

(= (my-max-if -60 -15) -15)
(= (my-max-if 15 60) 60)
(= (my-max-if 60 15) 60)

(= (my-max-guard 15 60) 60)
(= (my-max-guard 60 15) 60)