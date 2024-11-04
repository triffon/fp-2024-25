#lang racket

(define (get-smallest xs)
  (apply min xs)
  )

(= (get-smallest '(1 2 5)) 1)
(= (get-smallest '(2 1 5)) 1)
(= (get-smallest '(2 1 -1 5)) -1)