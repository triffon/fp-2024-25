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
(define (sum lst)
  (foldl* + 0 lst))

; 1. Намира дължина на списък
; Пример: (length* '(1 2 3 4)) ~> 4
(define (length* lst)
  (foldl* (lambda (nv _x) (+ nv 1)) 0 lst))

; 2. Връща конкатенацията на lst1 и lst2.
; Пример: (append* '(1 2 3) '(4 5)) ~> '(1 2 3 4 5)
(define (append* lst1 lst2)
  (foldr* cons lst2 lst1))

; 3. Връща lst след прилагане на f върху всеки елемент.
; Пример: (map* (lambda (x) (expt x 2)) '(1 2 3 4)) ~> '(1 4 9 16)
; Пример: (map* even? '(1 2 3 4)) ~> '(#false #true #false #true)
(define (map* f lst)
  (foldr* (lambda (x nv) (cons (f x) nv)) '() lst))

; 4. Връща списък от елементите на lst, за които предиката p е верен
; Пример: (filter* even? '(1 2 3 4 5 6)) ~> '(2 4 6)
; Пример: (filter* odd? '(2 4 6)) ~> '()
(define (filter* p lst)
  (foldr* (lambda (x nv)
            (if (p x) (cons x nv) nv))
          '()
          lst))

; 5. Премахва повторенията на елементи в lst
; Пример: (uniques '(1 1 3 1 5 6 5 5 2)) ~> '(3 1 6 5 2) или '(1 3 5 6 2)
;
; HINT: member
(define (uniques lst)
  (foldr* (lambda (el res)
            (if (member el res) res (cons el res)))
          '()
          lst))

; 6. Проверява дали p? е верен за точно n елемента от lst
; Пример: (sat-n? even? 3 '(4 1 3 5 6 2)) ~> #t
; Пример: (sat-n? even? 2 '(1 3 1 5 6 5)) ~> #f
(define (sat-n? p? n lst)
  (define (op x acc)
    (if (p? x)
        (+ 1 acc)
        acc))
  (= n (foldr* op 0 lst)))

; 7. Връща списък с елементите на lst, но в обратен ред.
; Пример: (reverse* '(1 2 3 4 5)) ~> '(5 4 3 2 1)
(define (flip binary-op)
  (lambda (x y) (binary-op y x)))

(define (reverse* lst)
  (foldl* (flip cons) '() lst))

; TODO: Fix
; 8. Намира броя на елементите в дълбокия списък lst.
; Тоест lst може да има произволни нива на вложеност.
;
; Пример: (count-atoms '(1 (2 3) () (4 (5 (6))) 7)) ~> 7
;
; HINT: recursion
(define (count-atoms lst)
  (define (op x acc)
    (cond ((null? x) 0)
          ((list? x) (+ acc (count-atoms x)))
          (else (+ acc 1))))
  (foldr* op 0 lst))

; TODO: Fix
; 9. Връща наредена двойка (fst . snd), където:
; fst - елементите за които p? е истина
; snd - елементите за които p? е лъжа
;
; Пример: (partition-i even? '(1 2 3 4 5 6 7)) ~> '((2 4 6) 1 3 5 7)
(define (partition-i p? lst)
  (define (op x acc)
    (if (p? x)
        (cons (cons x (car acc))
              (cdr acc))
        (cons (car acc)
              (cons x (cdr acc)))))
  (foldl* op '(()) lst))

