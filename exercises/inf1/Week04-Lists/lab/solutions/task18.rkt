#lang racket

(define (my-flatten xss)
  (cond
    [(empty? xss) xss]
    [(list? (car xss)) (append (my-flatten (car xss)) (my-flatten (cdr xss)))]
    [else (cons (car xss) (my-flatten (cdr xss)))]
    )
  )

(equal? (my-flatten '((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12)))))) '(1 2 3 4 5 6 7 8 9 10 11 12))