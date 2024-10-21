#lang racket

(define (help-galena new-views lost-views desired-views)
  (define (helper result current-views)
    (if (>= current-views desired-views)
        result
        (helper (add1 result) (+ new-views (- current-views lost-views)))
        )
    )
  (helper 1 new-views)
  )

(= (help-galena 100000 10000 910000) 10)