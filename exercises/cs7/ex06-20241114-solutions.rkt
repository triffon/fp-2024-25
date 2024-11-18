#lang racket
; Голямо контролно 1, решения (вариант А)

; Зад.1
; За целта на тази задача е удобно да считаме 0 за едноцифрено число
(define (count-digits n)
  (if (< n 10) 1
      (+ 1 (count-digits (quotient n 10)))))

(define (append-numbers n1 n2)
  (+ (* n1 (expt 10 (count-digits n2))) n2))
     
(define (multiply-by-position n)
  (define (helper n index)
    (if (= n 0) 0
        (append-numbers (helper (quotient n 10) (+ index 1))
                        (* (remainder n 10) index))))
  (helper n 1))

; Зад.2
(define (maximum lst)
  (apply max lst))
(define (minimum lst)
  (apply min lst))

(define (all-with-all fs xs)
  (maximum (map (lambda (f) (maximum (map f xs)))
                fs)))

(define (minmax fm l)
  (minimum (map (lambda (row) (all-with-all row l))
                fm)))

(define (1+ x) (+ x 1))
(define (square x) (* x x))

; Зад.3
(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define (all-paths t)
  (cond [(empty-tree? t) '()] ; празно дърво
        [(and (empty-tree? (left-tree t)) ; листо
              (empty-tree? (right-tree t)))
         (list (list (root-tree t)))]
        [else
         (append (list (list (root-tree t))) ; за "частични" пътища, не само до листа
                 (map (lambda (path) (cons (root-tree t) path))
                      (all-paths (left-tree t)))
                 (map (lambda (path) (cons (root-tree t) path))
                      (all-paths (right-tree t))))]))

(define (sum-evens lst)
  (apply + (filter even? lst)))

(define (best-by cmp lst)
  (foldr cmp (car lst) (cdr lst)))

(define (choose-path lst1 lst2)
  (cond [(> (sum-evens lst1) (sum-evens lst2)) lst1]
        [(< (sum-evens lst1) (sum-evens lst2)) lst2]
        [(> (length lst1) (length lst2)) lst1]
        [else lst2]))

(define (maximum-even-nodes-sum t)
  (best-by choose-path (all-paths t)))
