#lang racket

;; Помощни функции
(define (id x) x)
(define (++ n) (+ n 1))
(define (-- n) (- n 1))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))


;; Примерни решения

;; 1
(define (len l)
  (if (null? l)
      0
      (+ 1 (len (cdr l)))))

;; 2
; Така дефинирана, функцията ще хвърли грешка, когато `l` е празен списък
(define (minimum l)
  (define (helper acc l)
    (if (null? l)
        acc
        (helper (min acc (car l)) (cdr l))))
  (helper (car l) (cdr l)))

;; 3
(define (any? p l)
  (if (null? l)
      #f
      (or (p (car l)) (any? p (cdr l)))))

(define (all? p l)
  (not (any? (compose not p) l)))

;; 4
(define (member? x l)
  (if (null? l)
      #f
      (or (equal? x (car l)) (member? x (cdr l)))))

;; Можем също така да използваме `any`
(define (member?-any x l)
  (any? (lambda (el) (equal? el x)) l))

;; 5
(define (at n l)
  (cond
    [(< n 0)    #f]
    [(null? l)  #f]
    [(= n 0)    (car l)]
    [else       (at (- n 1) (cdr l))]))

;; 6
(define (push-back x l)
  (if (null? l)
      (list x)
      (cons (car l) (push-back x (cdr l)))))

;; 7
(define (reverse l)
  (define (helper acc l)
    (if (null? l)
        acc
        (helper (cons (car l) acc) (cdr l))))
  (helper (list) l))

;; 8
(define (insert x n l)
  (cond
    [(<= n 0) (cons x l)]
    [(null? l) (list x)]
    [else (cons (car l) (insert x (- n 1) (cdr l)))]))

;; 9
(define (range a b)
  (if (> a b)
      (list)
      (cons a (range (+ a 1) b))))

(define (range-acc a b)
  (accumulate cons (list) a b id ++))


;; Бонус задачи

;; 10
(define (map f l)
  (if (null? l)
      (list)
      (cons (f (car l)) (map f (cdr l)))))

;; 11
(define (filter p l)
  (if (null? l)
      (list)
      (let ([head (car l)]
            [tail-filtered (filter p (cdr l))])
        (if (p head)
            (cons head tail-filtered)
            tail-filtered))))

;; 12
(define (reduce op init l)
  (if (null? l)
      init
      (op (car l) (reduce op init (cdr l)))))

(define (map-via-reduce f l)
  (reduce (lambda (x l) (cons (f x) l)) (list) l))

(define (filter-via-reduce p l)
  (reduce (lambda (x l) (if (p x) (cons x l) l)) (list) l))
