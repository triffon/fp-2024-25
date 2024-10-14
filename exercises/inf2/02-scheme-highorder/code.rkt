#lang racket

(define (o f g)
  (define (comp-f-g x)
    (f (g x)))

  comp-f-g)

(define (o2 f g)
  (lambda (x) (f (g x))))

(define (double x)
  (* 2 x))

(define (add-1 x)
  (+ 1 x))

(define ladd-1 (lambda (x)
                 (+ 1 x)))

(define (repeated n f x)
  (if (= n 0)
      x
      (repeated (- n 1) f (f x))))

(define (repeat1 n f)
  (lambda (x) (repeated n f x)))

(define (repeat2 n f)
  (if (= n 0)
      (lambda (x) x)
      (o f (repeat2 (- n 1) f))))

(define (repeat3 n f)
  (if (= n 0)
      (lambda (x) x)
      (lambda (x) (f ((repeat3 (- n 1) f) x)))))

(define (repeat4 n f)
  (repeated n (lambda (g) (o f g)) (lambda (x) x)))

(define (accumulate-n op init f begin end)
  (if (> begin end)
      init
      (op (f begin) (accumulate-n op init f (+ 1 begin) end))))

(define (fact x)
  (accumulate-n * 1 (lambda (x) x) 1 x))

(define (count p a b)
  (accumulate-n
   +  ; op
   0  ; init
   (lambda (x) (if (p x) 1 0))  ; f
   a b))

(define (my-or x y)
  (if x #t y))

(define (exists? p a b)
  (accumulate-n
   my-or
   #f
   p
   a b))

(define (repeat5 n f)
  (accumulate-n
   o
   (lambda (x) x)
   (lambda (x) f)
   1 n))