#lang racket

; Задача 1
(define (cartesian-product lst1 lst2)
  (apply append
         (map (lambda (x)
                (map (lambda (y) (cons x y)) lst2))
              lst1)))

; Задача 2
(define (accumulate-i from to op nv)
  (if (> from to)
      nv
      (accumulate-i (+ 1 from) to op (op nv from))))

(define (excluded n)
  (define s (/ (* n (+ n 1)) 2))
  (define (excluded1 x)
    (accumulate-i
     1
     n
     (lambda (nv y)
       (if (= (- s (+ x y))
              (* x y))
           (cons (cons x y) nv)
           nv))
     '()))
  (accumulate-i 1
                n
                (lambda (nv x)
                  (append (excluded1 x) nv))
                '()))
