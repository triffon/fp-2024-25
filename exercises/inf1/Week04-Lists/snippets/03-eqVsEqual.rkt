#lang racket

(define a 3)
(define b 3)
(define c a)

(eq? a b)
(eq? a c)

(define d '(1 . 2))
(define e '(1 . 2))
(define f d)

(eq? d e)
(eq? d f)

(equal? d e)

(remq '(1 . 2) '((1 . 2) (4 . 5))) ; uses eq
(remove '(1 . 2) '((1 . 2) (4 . 5))) ; uses equal
(remq* (list (cons 1 2) (cons 4 5)) '((1 . 2) (4 . 5) (1 . 2))) ; uses eq
(remove* (list (cons 1 2) (cons 4 5)) '((1 . 2) (4 . 5) (1 . 2))) ; uses equal