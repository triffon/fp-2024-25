#lang racket

(display "Enter a number:")
(define input (read-line))

(displayln "You entered:")
(displayln input)

(+ 3 (string->number input))