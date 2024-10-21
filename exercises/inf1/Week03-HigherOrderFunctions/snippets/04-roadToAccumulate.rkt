#lang racket

; Expression 1
; 1 + 2 + 3 + ... + n

(define (expression1 a b)
  (define (helper current result)
    (if (> current b)
        result
        (helper (+ current 1) (+ result current))
        )
    )
  (helper a 0)
  )
(expression1 1 100)

; Expression 2
; 1 * 2 * 3 * ... * n

(define (expression2 a b)
  (define (helper current result)
    (if (> current b)
        result
        (helper (+ current 1) (* result current))
        )
    )
  (helper a 1)
  )

(expression2 1 5)

(define (smart-function1 a b nv op)
  (define (helper current result)
    (if (> current b)
        result
        (helper (+ current 1) (op result current))
        )
    )
  (helper a nv)
  )

(define (expression1-with-smart-function1 a b)
  (smart-function1 a b 0 +)
  )

(define (expression2-with-smart-function1 a b)
  (smart-function1 a b 1 *)
  )

(expression1-with-smart-function1 1 100)
(expression2-with-smart-function1 1 5)

; Expression 3
; 1 * 3 * 5 * ... * n

(define (expression3 a b)
  (define (helper current result)
    (if (> current b)
        result
        (helper (+ current 2) (* result current))
        )
    )
  (helper a 1)
  )

(expression3 1 5)

(define (smart-function2 a b nv op next)
  (define (helper current result)
    (if (> current b)
        result
        (helper (next current) (op result current))
        )
    )
  (helper a nv)
  )

(define (expression1-with-smart-function2 a b)
  (smart-function2 a b 0 + (λ (x) (+ x 1)))
  )

(define (expression2-with-smart-function2 a b)
  (smart-function2 a b 1 * (λ (x) (+ x 1)))
  )

(define (expression3-with-smart-function2 a b)
  (smart-function2 a b 1 * (λ (x) (+ x 2)))
  )

(expression1-with-smart-function1 1 100)
(expression2-with-smart-function2 1 5)
(expression3-with-smart-function2 1 5)

; Expression 1
; 1^2 + 3^2 + 5^2 + ... + n^2

(define (expression4 a b)
  (define (helper current result)
    (if (> current b)
        result
        (helper (+ current 2) (+ result (* current current)))
        )
    )
  (helper a 0)
  )

(expression4 1 5)

(define (smart-function3 a b nv op next term)
  (define (helper current result)
    (if (> current b)
        result
        (helper (next current) (op result (term current)))
        )
    )
  (helper a nv)
  )

(define (expression1-with-smart-function3 a b)
  (smart-function3 a b 0 + (λ (x) (+ x 1)) (λ (x) x))
  )

(define (expression2-with-smart-function3 a b)
  (smart-function3 a b 1 * (λ (x) (+ x 1)) (λ (x) x))
  )

(define (expression3-with-smart-function3 a b)
  (smart-function3 a b 1 * (λ (x) (+ x 2)) (λ (x) x))
  )

(define (expression4-with-smart-function3 a b)
  (smart-function3 a b 0 + (λ (x) (+ x 2)) (λ (x) (* x x)))
  )

(expression1-with-smart-function3 1 100)
(expression2-with-smart-function3 1 5)
(expression3-with-smart-function3 1 5)
(expression4-with-smart-function3 1 5)