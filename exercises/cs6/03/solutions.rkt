#lang racket

; NOTE: Суфикса * е защото съществуват вградени процедури
;       със същите имена.


; NOTE: take и drop не се държат така попринцип в racket,
; при по-голямо n биха хвърлили грешка.
; В Haskell се държат точно както са написани тук.

; 0. Намира дължината на списък
(define (length* lst)
  (if (null? lst)
      0
      (+ 1 (length* (cdr lst)))))

; 1. Връща списък от първите n елемента
(define (take* n lst)
  (if (or (null? lst) (zero? n))
      '()
      (cons (car lst)
            (take* (- n 1) (cdr lst)))))

; 2. Връща списък като lst, но без първите n елемента
(define (drop* n lst)
  (if (or (null? lst) (zero? n))
      lst
      (drop* (- n 1) (cdr lst))))

; 3. from-to
(define (from-to a b)
  (if (> a b)
      '()
      (cons a (from-to (+ a 1) b))))

; 4. По даден списък от числа - намира сумата им.
(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst)
         (sum (cdr lst)))))

; 5. Връща последния елемент на списъка lst.
(define (last* lst)
  (if (null? (cdr lst))
      (car lst)
      (last* (cdr lst))))

; 6. Връща n-тия елемент на списъка lst.
(define (nth n lst)
  (define (help i lst)
    (cond ((null? lst) 'not-found)
          ((= i n) (car lst))
          (else (help (+ 1 i) (cdr lst)))))
  (help 0 lst))

(define (nth* n lst)
  (if (zero? n)
    (car lst)
    (nth* (- n 1) (cdr lst))))

; 7. zip
(define (zip l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (cons (car l1) (car l2))
            (zip (cdr l1) (cdr l2)))))

; 8. Връща конкатенацията на lst1 и lst2.
(define (append* lst1 lst2)
  (if (null? lst1)
      lst2
      (cons
       (car lst1)
       (append* (cdr lst1) lst2))))

; 9. Прилага fn над елементите на lst, връща новия списък.
(define (map* f lst)
  (if (null? lst)
      lst
      (cons
       (f (car lst))
       (map* f (cdr lst)))))

; 10. Връща списък от елементите на lst,
; за които pred е вярно.
(define (filter* p lst)
  (cond ((null? lst) lst)
        ((p (car lst))
         (cons
          (car lst)
          (filter p (cdr lst))))
        (else (filter p (cdr lst)))))

; 11. Като функцията accumulate, но за списъци
; Пример: (foldl* - 0 '(1 2 3 4)) -> -10
(define (foldl* op acc lst)
  (if (null? lst)
      acc
      (foldl* op
              (op acc (car lst))
              (cdr lst))))

; 12. Като foldl, но рекурсивно
; Забележка: Началната стойност е десният аргумент на операцията
; Пример: (foldr* - 0 '(1 2 3 4)) -> -2
(define (foldr* op acc lst)
  (if (null? lst)
      acc
      (op (car lst)
          (foldr* op acc (cdr lst)))))
