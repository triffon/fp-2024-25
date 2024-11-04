(define dl '((1 2) ((3 (4) 5) (((6)))) (7)))

(define (atom? l)
  (and (not (null? l)) (not (pair? l))))

(define (count-atoms lst)
  (cond ((null? lst) 0)
        ((atom? lst) 1)
        (else (+ (count-atoms (car lst))
                 (count-atoms (cdr lst))))))

(define (deep-foldr op nv term lst)
  (cond ((null? lst) nv)
        ((atom? lst) (term lst))
        (else (op (deep-foldr op nv term (car lst))
                  (deep-foldr op nv term (cdr lst))))))

(define (deep-map f lst)
  (deep-foldr cons '() f lst))

(define (deep-member? x lst)
  (deep-foldr (lambda (a b) (or a b))
              #f
              (lambda (a) (equal? a x))
              lst))

(define (flatten lst)
  (deep-foldr append '() list lst))

(define (make-tree root left right) (list root left right))
(define (make-leaf root) (list root '() '()))
(define root car)
(define left cadr)
(define right caddr)
(define empty? null?)

(define t (make-tree 5
                     (make-tree 1
                                (make-tree 4
                                           '()
                                           (make-leaf 13))
                                (make-leaf 3))
                     (make-tree 8
                                (make-tree 0
                                           (make-leaf 10)
                                           (make-leaf 9))
                                (make-leaf 11))))

(define (height tree)
  (if (empty? tree) 0
      (+ 1 (max (height (left tree))
                (height (right tree))))))

(define (leaf? tree)
  (and (not (empty? tree))
       (empty? (left tree))
       (empty? (right tree))))

(define (count-leaves tree)
  (cond ((empty? tree) 0)
        ((leaf? tree) 1)
        (else (+ (count-leaves (left tree))
                 (count-leaves (right tree))))))

(define (map-tree f tree)
  (if (empty? tree) '()
      (make-tree (f (root tree))
                 (map-tree f (left tree))
                 (map-tree f (right tree)))))

(define (tree-to-list tree)
  (if (empty? tree) '()
      (append (tree-to-list (left tree))
              (list (root tree))
              (tree-to-list (right tree)))))

(define (level n tree)
  (cond ((empty? tree) '())
        ((zero? n) (list (root tree)))
        (else (append (level (- n 1) (left tree))
                      (level (- n 1) (right tree))))))

(define (fcons h t)
  (if (not (and h t))
      #f (cons h t)))

(define (path-to x tree)
  (cond ((empty? tree) #f)
        ((equal? x (root tree)) (list x))
        (else (fcons (root tree)
                     (or (path-to x (left tree))
                         (path-to x (right tree)))))))

(define derivation-tree (make-tree +
                                   (make-tree -
                                              (make-tree +
                                                         (make-leaf 5)
                                                         (make-leaf 7))
                                              (make-leaf 3))
                                   (make-tree *
                                              (make-tree +
                                                         (make-tree /
                                                                    (make-leaf 20)
                                                                    (make-leaf 4))
                                                         (make-leaf 9))
                                              (make-leaf 6))))

(define (calculate tree)
  (if (leaf? tree)
      (root tree)
      ((root tree)
       (calculate (left tree))
       (calculate (right tree)))))






  



