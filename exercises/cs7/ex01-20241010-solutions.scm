; Зад.1
(define (sum-interval a b)
  ; Инвариант: res = сумата на числата от 1 до i-1, вкл.
  (define (loop res i)
    (if (> i b)
        res
        (loop (+ res i) (+ i 1))))
  
  (loop 0 a))

; Зад.2
(define (count-digit d n)
  ; Инвариант: count = броя срещания на d в "обработената" част от n
  ; (вътрешното n е "оставащата" част)
  (define (loop count n)
    (cond ((= n 0) count)
          ((= d (remainder n 10)) (loop (+ count 1)
                                        (quotient n 10)))
          (else (loop res (quotient n 10)))))
  (loop 0 n))

; Зад.3
(define (reverse-digits n)
  ; Инвариант: res = цифрите, които са премахнати
  ;  от"оригиналното" n, в обратен ред
  (define (loop res n)
    (if (= n 0)
        res
        (loop (+ (* res 10) (remainder n 10))
              (quotient n 10))))
  (loop 0 n))

; Зад.4
(define (divisors-sum n)
  ; Инвариант: sum = сбора на делителите на n измежду 1 и i-1, вкл.
  (define (loop i sum)
;    (if (> i n)
;        sum
;        (loop (+ i 1)
;              (if (= 0 (remainder n i)) (+ sum i) sum))))
    (cond ((> i n) sum)
          ((zero? (remainder n i)) (loop (+ i 1) (+ sum i)))
          (else (loop (+ i 1) sum))))
  (loop 1 0))

; Зад.5
(define (perfect? n)
  (= (divisors-sum n) (* 2 n)))

; Зад.6
(define (prime? n)
  ; Инвариант: n не се дели на нито едно число измежду 2 и i-1, вкл.
  (define (loop i)
    (cond ((> (* i i) n) #t) ; по-добре от (> i (sqrt n))
          ((zero? (remainder n i)) #f)
          (else (loop (+ i 1)))))
  ; Заб.: обработваме специалния случай за 1 извън цикъла,
  ; вместо да проверяваме за него на всяка итерация
  ;(if (= n 1) #f (loop 2))
  (and (> n 1) (loop 2)))

; Зад.7
(define (increasing? n)
  (define (helper n)
    (< (remainder n 10) (remainder (quotient n 10) 10)))
  ; Самата increasing? е опашково-рекурсивна, няма нужда от помощна функция :)
  (cond ((< n 10) #t)
        ((helper n) #f)
        (else (increasing? rest))))

; Зад.8
; Развитие на стандартната идея, при която на всяка итерация
; делим числото на две и остатъкът ни казва дали поредният бит
; трябва да е вдигнат.
(define (toBinary n)
  ; Инвариант: на k-тата итерация pow==10^k;
  ; res съдържа долните k "бита" от резултата;
  ; n = "оригиналното" n, делено на 2^k
  (define (loop n res pow)
    (cond ((= n 0) res)
          ((zero? (remainder n 2))
           (loop (quotient n 2) res (* pow 10)))
          (else
           (loop (quotient n 2) (+ res pow) (* pow 10)))))
  (loop n 0 1))

; Зад.9
; Аналогичен подход и за обратната трансформация
(define (toDecimal n)
  ; Аналогизен инвариант
  (define (loop n res pow)
    (cond ((= n 0) res)
          ((zero? (remainder n 10)) ; тази клауза всъщност е ненужна :)
           (loop (quotient n 10) res (* pow 2)))
          (else
           (loop (quotient n 10) (+ res (* (remainder n 10) pow)) (* pow 2)))))
  (loop n 0 1))

; Зад.10
(define (sq x) (* x x))
(define (fast-exp x n)
  (cond ((= n 0) 1)
        ((even? n) (sq (fast-exp x (/ n 2))))
        (else (* x (sq (fast-exp x (quotient n 2)))))))
