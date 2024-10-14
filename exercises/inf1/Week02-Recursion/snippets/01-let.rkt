#lang racket

(define (help-gosho breakfast+lunch)
  (let ([dinner (- 15 breakfast+lunch)])
  (cond
    [(and (>= dinner 10) (<= dinner 15)) "Pizza"]
    [(>= dinner 5) "Doner"]
    [(>= dinner 3) "University cafeteria"]
    [else "Hungry :'("]
    ))
  )

(equal? (help-gosho 0) "Pizza")
(equal? (help-gosho 7.5) "Doner")
(equal? (help-gosho 10.01) "University cafeteria")