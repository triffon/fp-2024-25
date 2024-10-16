(define (fast-expt base exponent)
    (define (square x) (* x x))
    (define (for b e product)  ; тук base и exponent от for "скриват" аргументите от fast-expt листа с аргументи
        (cond 
            ((= e 0) 
                (if (< exponent 0) (/ 1 product) product)
            )
            ((even? e)
                (for (square b) (/ e 2) product)
            )
            (else 
                (for b (- e 1) (* product b))
            )
        )
    )
    (for base (abs exponent) 1)
)
