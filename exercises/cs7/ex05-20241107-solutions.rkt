#lang racket
; Стандартен интерфейс за работа с двоични дървета
(define empty-tree '())
(define (make-tree root left right) (list root left right))      ; не искаме просто (define make-tree list) - защо?
(define (make-leaf root) (make-tree root empty-tree empty-tree)) ; за удобство
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

; Зад.1
(define (height t)
  (if (empty-tree? t) 0
      (+ 1 (max (height (left-tree t))
                (height (right-tree t))))))

; Зад.2
(define (get-level n t)
  (cond [(empty-tree? t) '()]
        [(= n 0) (list (root-tree t))]
        [else (append (get-level (- n 1) (left-tree t))
                      (get-level (- n 1) (right-tree t)))]))

; Зад.3
(define (maxLength l1 l2)
  (if (> (length l1) (length l2)) l1 l2))

; Важно - да имаме само по едно извикване за всяко от поддърветата
; Алтернативно - може да "запазим" резултатите с let
(define (find-longest-path t)
  (if (empty-tree? t) '()
      (cons (root-tree t)
            (maxLength (find-longest-path (left-tree t))
                       (find-longest-path (right-tree t))))))
