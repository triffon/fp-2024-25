#lang racket

(define (at l i)
  (if (null? l)
      #f
      (if (<= i 0)
          (car l)
          (at (cdr l) (- i 1)))))

(define (len l)
  (if (null? l)
      0
      (+ 1 (len (cdr l)))))

(define (forall? p l)
  (if (null? l)
      #t
      (and (p (car l)) (forall? p (cdr l)))))

; matrices

(define mymat '((1 2 3) (4 5 6) (7 8 9)))

(define (mat-at m i j)
  (at (at m i) j))

(define (mat-map f m)
  (map (lambda (l) (map f l)) m))

(define (mat-forall? p m)
  (forall? (lambda (l) (forall? p l)) m))

(define (mat? m)
  (and
   (list? m)
   (forall? list? m)
   (mat-forall? number? m)
   (or
    (null? m)
    (forall? (lambda (l) (= (len l) (len (car m)))) m))))

(define (scalmul x m)
  (mat-map (lambda (y) (* y x)) m))

(define (get-first-column m)
  (map car m))

(define (rm-first-column m)
  (map cdr m))

(define (transpose m)
  (if (or (null? m) (null? (car m)))
      '()
      (cons (get-first-column m)
            (transpose (rm-first-column m)))))

(define (zip-with f l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (f (car l1) (car l2)) (zip-with f (cdr l1) (cdr l2)))))

(define (zip-with2 f l1 l2) (map f l1 l2))

(define (sum l)
  (foldr + 0 l))

(define (sum2 l) (apply + l))

(define (scal-vec-mul v1 v2)
  (sum (zip-with * v1 v2)))

(define (mulrows m n)
  (if (or (null? m) (null? n))
      '()
      (cons
       (map (lambda (col) (scal-vec-mul (car m) col)) n)
       (mulrows (cdr m) n))))

(define (mulrows2 m n)
  (map (lambda (l) (map (lambda (col) (scal-vec-mul l col)) n)) m))

(define (matmul m n)
  (mulrows m (transpose n)))

(define (matmul2 m n)
  (mulrows2 m (transpose n)))