#lang racket

; 1. За дадено естествено число n намира n+1.
(define (succ x) (+ x 1))

; 2. За дадено естествено число n намира n-1.
(define (pred x) (- x 1))

; 3. За дадено число n, връща:
; - n/2 ако n е четно
; - n в противен случай
(define (safe-div x)
  (if (even? x)
    (/ x 2)
    x))

; 4. Намира n!
(define (factorial n)
  (if (= n 0) 1 (* n (factorial (- n 1)))))

; 5. Намира n-тото число на Фибоначи.
(define (fibonacci n)
  (if (< n 2)
    n
    (+ (fibonacci (- n 1))
       (fibonacci (- n 2)))))

; 6. Намира сумата на 2 естествени числа.
; Използвайте succ и pred.
(define (sum x y)
  (if (zero? x)
    y
    (sum (pred x) (succ y))))

; 7. намира произведението на 2 естествени числа.
; Използвайте sum и pred.
(define (product x y)
  (cond ((= 0 x) 0)
        ((= 1 x) y)
        (else (sum y(product (pred x) y)))))

; 8. За даден едноместен предикат p, връща отрицанието му.
; Не отрицанието на резултата, а нов предикат
; който е отрицание на p.
(define (complement p)
  (lambda (x) (not (p x))))

; 9. За дадена функция на два аргумента f,
; връща функцията над разменени аргументи.
(define (flip f)
  (lambda (x y) (f y x)))

; 10. За дадени едноаргументни функции f и g,
; връща композицията им (f.g)
(define (compose f g)
  (lambda (x) (f (g x))))

; 11. За дадена едноаргументна функция f и число n,
; връща n-тото прилагане на f. Тоест f^n.
; Пример: ((repeat f 3) x) -> (f (f (f x))) за някоя f
(define id (lambda (x) x))
(define (repeat f n)
  (if (zero? n)
    id
    (lambda (x)
      ((repeat f (- n 1)) (f x)))))
