#lang racket

(define (assoc-del key xs)
  (cond
    [(empty? xs) '()]
    [(equal? (caar xs) key) (cdr xs)]
    [else (cons (car xs) (assoc-del key (cdr xs)))]
    )
  )

(equal? (assoc-del 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) '((2 . "Two") (3 . "Three")))
(equal? (assoc-del 1 (list '(2 . "Two") '(3 . "Three"))) '((2 . "Two") (3 . "Three")))