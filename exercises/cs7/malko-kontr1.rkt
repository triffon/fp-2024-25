#lang racket
(define (take n lst)
  (cond [(null? lst) '()]
        [(= n 0) '()]
        [else (cons (car lst)
                    (take (- n 1) (cdr lst)))]))

(define (all? p? lst)
  (or (null? lst)
      (and (p? (car lst))
           (all? p? (cdr lst)))))

(define (complement p?)
  (lambda (x) (not (p? x))))
(define (any? p? lst)
  (not (all? (complement p?) lst)))

(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2)) '()
      (cons (cons (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2)))))

; Зад.1, директно решение
(define (sum lst)
  ;(foldr + 0 lst))
  (apply + lst))

(define (moving-average lst k)
  ; Итерацията спира след n стъпки, когато дължината на lst е точно k
  (define (helper lst n)
    (if (= n 0) '()
        (cons (/ (sum (take k lst)) k)
              (helper (cdr lst) (- n 1)))))
  (helper lst (+ 1 (- (length lst) k))))

; Зад.2
(define (sublist? needle haystack)
  (if (null? haystack) (null? needle)
      (or (starts-with? needle haystack)
          (sublist? needle (cdr haystack)))))

(define (starts-with? l1 l2)
  (equal? l1 (take (length l1) l2)))

; Зад.3
(define (majors? l1 l2)
  ; Проверката за дължините не е задължителна,
  ; може да я приемем като предусловие
  (and (= (length l1) (length l2))
       (all? (lambda (p) (<= (car p) (cdr p)))
             (zip l1 l2))))

; Ключ към зад.1 и зад.4 - т.нар. sliding window
(define (slide lst k)
  (define (helper lst n)
    (if (= n 0) '()
        (cons (take k lst)
              (helper (cdr lst) (- n 1)))))
  (helper lst (+ 1 (- (length lst) k))))

; Зад.1, бонус решение
(define (moving-average* lst k)
  (map (lambda (l) (/ (sum l) k))
       (slide lst k)))

; Зад.4
(define (any-majors? a b)
  (any? (lambda (l) (majors? a l)) 
        (slide b (length a))))
