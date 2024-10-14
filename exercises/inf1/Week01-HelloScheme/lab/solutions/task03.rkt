#lang racket

(define (last-digit n) (remainder n 10))

(= (last-digit 176) 6)
(= (last-digit 0) 0)