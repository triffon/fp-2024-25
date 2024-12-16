#lang racket

(define (atom? n) (not (list? n)))

(define (deep-replace xss)
  (define (helper xss curDepth)
    (cond
      [(atom? xss) (if (> xss curDepth) curDepth xss)]
      [(null? xss) '()]
      [else (cons (helper (car xss) (add1 curDepth)) (helper (cdr xss) curDepth))]))
  (helper xss 0))

(equal? (deep-replace '(1 (3 (4 2) 1) 5 (6 (1)))) '(1 (2 (3 2) 1) 1 (2 (1))))
