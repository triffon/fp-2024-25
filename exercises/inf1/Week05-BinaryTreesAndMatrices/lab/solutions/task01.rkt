#lang racket

(define our-cool-tree '(1 (2 () ())
                          (3 (4 () ())
                             (5 () ()))))

(define (root bt) (car bt))
(define (left-subtree bt) (cadr bt))
(define (right-subtree bt) (caddr bt))

(define (inorder bt)
  (if (empty? bt)
      '()
      (append (inorder (left-subtree bt)) (list (root bt)) (inorder (right-subtree bt)))
      )
  )

(define (preorder bt)
  (if (empty? bt)
      '()
      (append (list (root bt)) (preorder (left-subtree bt)) (preorder (right-subtree bt)))
      )
  )

(define (postorder bt)
  (if (empty? bt)
      '()
      (append (postorder (left-subtree bt)) (postorder (right-subtree bt)) (list (root bt)))
      )
  )

(equal? (inorder our-cool-tree) '(2 1 4 3 5))
(equal? (preorder our-cool-tree) '(1 2 3 4 5))
(equal? (postorder our-cool-tree) '(2 4 5 3 1))