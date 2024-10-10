#lang racket

(define (sum-interval begin end)
  (if (> begin end)
      0
      (+ begin (sum-interval (+ 1 begin) end))))

(define (prod-interval begin end)
  (if (> begin end)
      1
      (* begin (prod-interval (+ 1 begin) end))))

(define (accumulate-interval op init begin end)
  (if (> begin end)
      init
      (op begin (accumulate-interval op init (+ 1 begin) end))))

(define (sum-interval2 begin end)
  (accumulate-interval + 0 begin end))

(define (prod-interval2 begin end)
  (accumulate-interval * 1 begin end))

(define (add-1 x) (+ 1 x))
(define (add-2 x) (+ 2 x))

(define (make-adder y)
  (define (adder x)
    (+ x y))

  adder)

(define (make-adder2 y)
  (lambda (x) (+ x y)))

(define add-3 (make-adder 3))

(define answer 42)

(define (fact x)          (accumulate-interval * 1 1 x))
(define fact2 (lambda (x) (accumulate-interval * 1 1 x)))

(define (square x) (* x x))
(define (double x) (* 2 x))

(define (o f g)
  (lambda (x) (f (g x))))

(define (repeated f n x)
  (if (= n 0)
      x
      (repeated f (- n 1) (f x))))

(define (repeat1 f n)
  (lambda (x) (repeated f n x)))

(define (repeat2 f n)
  (if (= n 0)
      (lambda (x) x)
      (lambda (x) ((o (repeat2 f (- n 1)) f) x))))

(define (repeat3 f n)
  (if (= n 0)
      (lambda (x) x)
      ((o (repeat3 f (- n 1)) f))))

(define (repeat4 f n)
  (repeated (lambda (g) (o f g)) n (lambda (x) x)))

(define (accumulate-n op init f begin end)
  (if (> begin end)
      init
      (op (f begin) (accumulate-n op init f (+ 1 begin) end))))

(define (count p a b)
  (accumulate-n
   +
   0
   (lambda (x) (if (p x) 1 0))
   a b))
   