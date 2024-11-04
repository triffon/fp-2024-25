#lang racket

; (null? xs)
; (empty? xs)
; (equal? xs '())
; (cons? xs)

(first '(1 2 3 4 5))
(second '(1 2 3 4 5))
(third '(1 2 3 4 5))
(fourth '(1 2 3 4 5))
; ... tenth
(last '(1 2 3 4 5))

(list-ref '(1 2 3 4 5) 3)

(length '(1 2 3))

(member 7 '(1 2 3 4 5))
(member 2 '(1 2 3 4 5))
(list? (member 2 '(1 2 3 4 5)))

(reverse '(1 2 3 4 5))

(append '(1 2 3) '(4 5 6) '(7 8 9) '(10 11 12) '(13))

(flatten '(1 2 (3 4 5) (6 (7 (8)))))

(remove-duplicates '(1 2 3 1 2 3 4 1 2 3 41 2 3 56))

(remq 1 '(2 1 1))
(remq* '(1) '(1 2 3 1))
(remq* '(1 2 3) '(1 2 3 1))

(range 5 10)
(range 5 10 2)