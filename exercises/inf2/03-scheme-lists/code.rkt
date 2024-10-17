#lang racket

(define (len l)
  (if (null? l)
      0
      (+ 1 (len (cdr l)))))