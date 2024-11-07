#lang racket

(define our-cool-tree '(1 (2 () ())
                          (3 (4 () ())
                             (5 () ()))))

(define (root bt) (car bt))
(define (left-subtree bt) (cadr bt))
(define (right-subtree bt) (caddr bt))

(define (min2 comparator a b)
  (if (comparator a b) a b)
  )

(define (min3 comparator a b c)
  (min2 comparator (min2 comparator a b) c)
  )

(define (min comparator bt)
  (cond
    [(empty? bt) (error "Empty tree")]
    [(and (empty? (left-subtree bt)) (empty? (right-subtree bt))) (root bt)] ; leaf
    [(empty? (left-subtree bt)) (min2 comparator (root bt) (min comparator (right-subtree bt)))] ; has only right subtree
    [(empty? (right-subtree bt)) (min2 comparator (root bt) (min comparator (left-subtree bt)))] ; has only left subtree
    [else (min3 comparator (root bt) (min comparator (left-subtree bt)) (min comparator (right-subtree bt)))]
    )
  )

(define (max2 comparator a b)
  (if (comparator a b) b a)
  )

(define (max3 comparator a b c)
  (max2 comparator (max2 comparator a b) c)
  )

(define (max comparator bt)
  (cond
    [(empty? bt) (error "Empty tree")]
    [(and (empty? (left-subtree bt)) (empty? (right-subtree bt))) (root bt)] ; leaf
    [(empty? (left-subtree bt)) (max2 comparator (root bt) (max comparator (right-subtree bt)))] ; has only right subtree
    [(empty? (right-subtree bt)) (max2 comparator (root bt) (max comparator (left-subtree bt)))] ; has only left subtree
    [else (max3 comparator (root bt) (max comparator (left-subtree bt)) (max comparator (right-subtree bt)))]
    )
  )


(= (min < our-cool-tree) 1)
(= (max < our-cool-tree) 5)