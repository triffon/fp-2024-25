#lang racket

(define (fact-iterative n)
  (define (helper current-n result)
    (if (<= current-n 1)
        result
        (helper (- current-n 1) (* result current-n)))
    )
  (helper n 1)
  )

(fact-iterative 5)
