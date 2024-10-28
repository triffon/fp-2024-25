#lang racket

;; Помощни функции
(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b)
      nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (++ n) (+ n 1))
(define (-- n) (- n 1))
(define (id x) x)


;; Функции от по-висок ред

;; 1
(define (compose f g)
  (lambda (x) (f (g x))))

;; 2
(define (const c)
  (lambda (x) c))

;; 3
(define (fmax f g)
  (lambda (x)
    ; Използваме `let`, за да пресментнем `(f x)` и `(g x)` само веднъж
    (let ((fx (f x))
          (gx (g x)))
      (if (> fx gx) fx gx))))

;; 4
(define (repeated n f x)
  (if (= n 0)
      x
      (f (repeated (-- n) f x))))

;; 5
(define (repeat n f)
  (lambda (x) (repeated n f x)))


;; Accumulate

;; 6
(define (count p? a b)
  (accumulate + 0 a b (lambda (x) (if (p? x) 1 0)) ++))

;; 7
(define (any p? a b)
  (accumulate (lambda (a b) (or a b)) #f a b p? ++))

;; По-кратко решение, което използва функцията `count` от предната задача
(define (any-2 p? a b)
  (> (count p? a b) 0))

;; 8
(define (all p? a b)
  (accumulate (lambda (a b) (and a b)) #t a b p? ++))

;; Вариант със `any`
(define (all-2 p? a b)
  (not (any (compose not p?) a b)))

;; 9
(define (repeat-acc n f)
  (accumulate compose id 1 n (const f) ++))

(define (repeated-acc n f x)
  ((repeat-acc n f) x))
