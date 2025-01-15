#lang racket
(define (atom? x) (not (pair? x)))

; Класическо решение с "двупосочна" рекурсия
(define (deepMapCond xss p f1 f2)
  (define (helper xss depth)
    (cond [(null? xss) '()]
          [(atom? (car xss))
           (cons ((if (p (car xss) depth) f1 f2) (car xss) depth)
                 (helper (cdr xss) depth))]
          [else (cons (helper (car xss) (+ depth 1))
                      (helper (cdr xss) depth))]))
  (helper xss 1))

(deepMapCond '(1 (2 (5 1) 4) 3) > (λ (x d) d) (λ (x d) (* x 2)))

; Решение, вдъхновено от хаскелското такова: атомите обработваме по условие,
; а върху вложените списъци просто викаме map на същата обработка.
(define (deepMapCond* xss p f1 f2)
  (define (helper x depth)
    (if (atom? x)
        ((if (p x depth) f1 f2) x depth)
        (map (lambda (y) (helper y (+ depth 1))) x)))
  (helper xss 0))

(deepMapCond* '(1 (2 (5 1) 4) 3) > (λ (x d) d) (λ (x d) (* x 2)))
