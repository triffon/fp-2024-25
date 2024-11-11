#lang racket

(define empty-binary-tree '())
(define (make-tree root left right) (list root left right))

(define (root bt) (car bt))
(define (left-subtree bt) (cadr bt))
(define (right-subtree bt) (caddr bt))

(define (binary-tree? t)
  (or (empty? t)
      (and (list? t) (= (length t) 3) (binary-tree? (left-subtree t)) (binary-tree? (right-subtree t))))
  )

