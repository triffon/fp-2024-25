#lang racket

;===============;=========================================
;; З А Д А Ч И ;;
;;;;;;;;;;;;;;;;;

; Решете всички задачи чрез извикване на foldl* или foldr*

(define (foldl* op nv lst)
  (if (null? lst)
      nv
      (foldl* op (op nv (car lst)) (cdr lst))))

(define (foldr* op nv lst)
  (if (null? lst)
      nv
      (op (car lst) (foldr* op nv (cdr lst)))))

; 0. По даден списък от числа - намира сумата им.
; Пример: (sum '(1 2 3 4)) ~> 10
(define (sum lst) 'undefined)

; 1. Намира дължина на списък
; Пример: (length* '(1 2 3 4)) ~> 4
(define (length* lst) 'undefined)

; 2. Връща конкатенацията на lst1 и lst2.
; Пример: (append* '(1 2 3) '(4 5)) ~> '(1 2 3 4 5)
(define (append* lst1 lst2) 'undefined)

; 3. Връща lst след прилагане на f върху всеки елемент.
; Пример: (map* (lambda (x) (expt x 2)) '(1 2 3 4)) ~> '(1 4 9 16)
; Пример: (map* even '(1 2 3 4)) ~> '(#false #true #false #true)
(define (map* f lst) 'undefined)

; 4. Връща списък от елементите на lst, за които предиката p е верен
; Пример: (filter* even? '(1 2 3 4 5 6)) ~> '(2 4 6)
; Пример: (filter* odd? '(2 4 6)) ~> '()
(define (filter* p lst) 'undefined)

; 5. Премахва повторенията на елементи в lst
; Пример: (uniques (1 1 3 1 5 6 5 5 2)) ~> '(3 1 6 5 2) или '(1 3 5 6 2)
;
; HINT: member
(define (uniques lst) 'undefined)

; 6. Проверява дали p? е верен за точно n елемента от lst
; Пример: (sat-n? even 3 (4 1 3 5 6 2)) ~> #t
; Пример: (sat-n? even 2 (1 3 1 5 6 5)) ~> #f
(define (sat-n? p? n lst) 'undefined)

; 7. Връща списък с елементите на lst, но в обратен ред.
; Пример: (reverse* (1 2 3 4 5)) ~> '(5 4 3 2 1)
(define (reverse* lst) 'undefined)

; 8. Намира броя на елементите в дълбокия списък lst.
; Тоест lst може да има произволни нива на вложеност.
;
; Пример: (count-atoms '(1 (2 3) () (4 (5 (6))) 7)) ~> 7
;
; HINT: recursion
(define (count-atoms lst) 'undefined)

; 9. Връща наредена двойка (fst . snd), където:
; fst - елементите за които p? е истина
; snd - елементите за които p? е лъжа
;
; Пример: (partition-i even '(1 2 3 4 5 6 7)) ~> '((2 4 6) 1 3 5 7)
(define (partition-i p? lst) 'undefined)

