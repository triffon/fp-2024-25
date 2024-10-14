#lang racket

(define (help-students x y)
  (cond
    [(and (zero? x) (zero? y)) "Center"]
    [(zero? x) "Ordinate"]
    [(zero? y) "Abscissa"]
    [(and (positive? x) (positive? y)) "1 Quadrant"]
    [(positive? x) "4 Quadrant"]
    [(positive? y) "2 Quadrant"]
    [else "3 Quadrant"]
    )
  )

(equal? (help-students 0 0) "Center")
(equal? (help-students 5 0) "Abscissa")
(equal? (help-students 0 4) "Ordinate")
(equal? (help-students 1 1) "1 Quadrant")
(equal? (help-students -2 -4) "3 Quadrant")
(equal? (help-students 2 -4) "4 Quadrant")
(equal? (help-students -2 4) "2 Quadrant")