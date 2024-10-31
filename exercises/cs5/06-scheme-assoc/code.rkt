#lang racket

(define ages (list
              (cons 'pesho 42)
              (cons 'ivan 56)
              (cons 'gogo #f)
              (cons 'gosho 712)))

(define not-ages (list
                  (cons 'pesho 42)
                  (cons 'ivan 56)
                  (cons 'pesho 12)
                  (cons 'gosho 712)))
                  

(define friends (list
                 (cons 'pesho (list 'gosho 'penka))
                 (cons 'ivan (list 'pesho 'ivan))
                 (cons 'gogo (list))))

(define (forall? p l)
  (if (null? l)
      #t
      (and (p (car l)) (forall? p (cdr l)))))

(define (exists? p l)
  (if (null? l)
      #f
      (or (p (car l)) (exists? p (cdr l)))))

(define (keys l)
  (map car l))

(define (member? x l)
  (exists? (lambda (y) (equal? x y)) l))

(define (unique? l)
  (if (null? l)
      #t
      (and
       (not (member? (car l) (cdr l)))
       (unique? (cdr l)))))

(define (assoc? x)
  (and
   (list? x)
   (unique? (keys x))
   (forall? pair? x)))

(define assoc-empty '())