#lang racket

(display "Enter some list elements separated by ' ': ")
(define input (read-line))

(newline)

(define result (string-split input))

(display result)