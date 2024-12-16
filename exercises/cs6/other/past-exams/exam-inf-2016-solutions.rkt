#lang racket

; Task 1
(define (foldl op nv lst)
  (if (null? lst)
      nv
      (foldl op (op nv (car lst)) (cdr lst))))

(define (count-p p lst)
  (define (op nv x)
    (if (p x) (+ nv 1) nv))
  (foldl op 0 lst))

(define (exists p lst)
  (positive? (count-p p lst)))

(define (forall p lst)
  (= (length lst) (count-p p lst)))

(define (transpose m) (apply map list m))

(define (findColumns m)
  (count-p (lambda (col)
             (exists        ; exists a row
              (lambda (row) ; such that
                (forall     ; all elements of the column are in that row
                 (lambda (x) (member x row)) col))
              m))
           (transpose m)))

(define m '((1 4 3) (4 5 6) (7 4 9)))
(findColumns m) ; should be 1

; Taks 2
(define (fhg f h g) (lambda (x) (h (f x) (g x))))

(define (fromTo a b)
  (if (> a b)
      '()
      (cons a (fromTo (+ a 1) b))))

; Going by the task's definition we must check the following predicate:
; (exists f in uns)(exists g in uns)(exists h in bins)
; (exists f1 in uns)(forall x in [a;b]) (fhg(x) = f1(x))
;
; Another way is to make the Cartesian product (uns x uns x bins x uns)
; And search for the quintuplet (f,g,h,f1)
; So we can do the same with a single call of "exists" afterwards,
; but i'm not sure how much shorter of a solution that would be.
(define (check a b uns bins)
  (exists
   (lambda (f)
     (exists
      (lambda (g)
        (exists
         (lambda (h)
           (exists
            (lambda (f1)
              (forall (lambda (x) (= ((fhg f h g) x) (f1 x)))
                      (fromTo a b)))
            uns))
         bins))
      uns))
   uns))

(check 1
       9
       (list (lambda (x) (+ x 1))
             (lambda (x) (- x 1))
             (lambda (x) (- (expt x 2) 1)))
       (list (lambda (x y) (* x y)))) ; should be #t

; Task 3
; plant :: (String, Int, Int)

; An aproach to this one is to go trough a Cartesian product of all the min and
; max temperatures, and then see which tuple (min, max) can host the most plants

; Get the surviving plants in a temperature range  (minT, maxT)
(define (survivors plants minT maxT)
  (filter (lambda (p)
            (and (> (caar p) minT) (< (caaar p) maxT)))
          plants))

; TODO
; (define (garden plants)
;   (let ((op (lambda (nv x) ())))
;     (foldl op '(0 . 0) plants)))

; Task 4 (TODO)


