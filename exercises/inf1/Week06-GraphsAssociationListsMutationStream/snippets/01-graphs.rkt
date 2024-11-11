#lang racket

(define empty-graph '())

(define my-graph-1 '((1 2 3)
                     (2 3)
                     (3 4 5)
                     (4)
                     (5 2 4 6)
                     (6 2)))

(define my-graph-2 '((1 . (2 3))
                     (2 . (3))
                     (3 . (4 5))
                     (4 . ())
                     (5 . (2 4 6))
                     (6 . (2))))

(display my-graph-1)
(display my-graph-2)