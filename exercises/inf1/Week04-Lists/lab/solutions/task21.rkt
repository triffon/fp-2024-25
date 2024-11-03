#lang racket

(define (deep-delete xss)
  (define (helper current-level left-over)
    (cond
      [(empty? left-over) left-over]
      [(list? (car left-over)) (cons (helper (add1 current-level) (car left-over)) (helper current-level (cdr left-over)))]
      [(>= (car left-over) current-level) (cons (car left-over) (helper current-level (cdr left-over)))]
      [else (helper current-level (cdr left-over))]
      )
    )
  (helper 1 xss)
  )

(equal? (deep-delete '(1 (2 (2 4) 1) 0 (3 (1)))) '(1 (2 (4)) (3 ())))