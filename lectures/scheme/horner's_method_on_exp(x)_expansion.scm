; Задачата изисква рекурсия.
; Рекурсивно обръщение към функция, докато още я конструираме (с lambda), е НЕвъзможно.
; За целта ни трябва тази фунцкия да пази себе си като параметър-функция, която да извикваме.

(define (calculate-e^x x order)
    (let* 
        (
            (1+ (lambda (x) (+ x 1)))
            (horners-method 
                (lambda (f iterator)
                    (if (> iterator order) 
                        1
                        (+ 1 (* (/ x iterator) (f f (1+ iterator))))
                    )
                )
            )
        )
        (if (= order 0) 1 (horners-method horners-method 1))
    )
)
