#lang racket

(define (len l)
  (if (null? l)
      0
      (+ 1 (len (cdr l)))))

(define (repeat n x)
  (if (< n 1)
      '()
      (cons x (repeat (- n 1) x))))

(define (exists? l p)
  (if (null? l)
      #f
      (if (p (car l))
          #t
          (exists? (cdr l) p))))

(define (member? l x)
  (exists? l (lambda (y) (equal? x y))))

(define (at n l)
  (if (or (null? l) (< n 0))
      #f
      (if (= n 0)
          (car l)
          (at (- n 1) (cdr l)))))

(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l)) (map f (cdr l)))))

(define (filter p l)
  (if (null? l)
      '()
      (if (p (car l))
          (cons (car l) (filter p (cdr l)))
          (filter p (cdr l)))))




(define (f x) (remainder (+ x 2) 5))

; f е биекция върху (list 0 1 2 3 4) и (list 0 2 4), но не върху (list 0 1 2 4)
; сюрекция: за всеки елемент y от списъка съществува елемент x така че (f x) == y
; инекция: за всеки елемент y от списъка не съществуват повече от един елемент x така че (f x) == y