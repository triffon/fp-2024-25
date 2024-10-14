#lang racket

(display "Hello, Scheme\n")

; comparison
(display "Comparison operators:\n")
(= 2 2) ; use '=' for numbers
(= 2 5)

(equal? "alo" "alo") ; use 'equal?' when using '=' is not possible
(equal? "zdr" "hi")

(> 5 2)
(<= 1 2 3 4 5)

; boolean operators
(display "Boolean operators:\n")
(not #t)
(and #t #t)
(or #t #f)

; arithmetic operators
(display "Arithmetic operators:\n")
(* 2 3)
(+ 1 2 3 4 5)
(/ 5 2)
(/ 5.0 2.0)
(quotient 5 2)
(remainder 5 2)
(expt 2 10)
; (/ 5 0)
