#lang racket
; Пример - вградена в ракет функция
(define (list-ref* lst idx)
  (cond [(null? lst) #f]
        [(zero? idx) (car lst)]
        [else (list-ref* lst (- idx 1))]))

; Зад.1
(define (take n lst)
  (cond [(null? lst) '()]
        [(= n 0) '()]
        [else (cons (car lst)
                    (take (- n 1) (cdr lst)))]))
(define (drop n lst)
  (cond [(null? lst) '()]
        [(= n 0) lst]
        [else (drop (- n 1) (cdr lst))]))

; Зад.2
; Напомняне: функции, връщащи bool, нямат нужда от if/cond
(define (all? p? lst)
  (or (null? lst)
      (and (p? (car lst))
           (all? p? (cdr lst)))))
; по ДеМорган
(define (complement p?)
  (lambda (x) (not (p? x))))
(define (any? p? lst)
  (not (all? (complement p?) lst)))

; Зад.3
(define (zip lst1 lst2)
  (zipWith cons lst1 lst2))

; Зад.4
(define (zipWith f lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (f (car lst1) (car lst2))
            (zipWith f (cdr lst1) (cdr lst2)))))

; Зад.5
; Когато достъпваме по два елемента на всяка итерация,
; трябва да сме проверили дали съществуват поне два.
(define (sorted? lst)
  (or (null? lst)
      (null? (cdr lst))
      (and (<= (car lst) (cadr lst))
           (sorted? (cdr lst)))))

; Зад.6
(define (id x) x)
(define (sorted?* lst)
  (all? id (zipWith < lst (cdr lst))))
