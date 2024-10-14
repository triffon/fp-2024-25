#lang racket

(define (thousands-to-roman thousands)
  (cond
    [(= thousands 1) "M"]
    [(= thousands 2) "MM"]
    [(= thousands 3) "MMM"]
    [else ""]
    )
  )

(define (hundreds-to-roman hundreds)
  (cond
    [(= hundreds 1) "C"]
    [(= hundreds 2) "CC"]
    [(= hundreds 3) "CCC"]
    [(= hundreds 4) "CD"]
    [(= hundreds 5) "D"]
    [(= hundreds 6) "DC"]
    [(= hundreds 7) "DCC"]
    [(= hundreds 8) "DCCC"]
    [(= hundreds 9) "CM"]
    [else ""]
    )
  )

(define (tens-to-roman tens)
  (cond
    [(= tens 1) "X"]
    [(= tens 2) "XX"]
    [(= tens 3) "XXX"]
    [(= tens 4) "XL"]
    [(= tens 5) "L"]
    [(= tens 6) "LX"]
    [(= tens 7) "LXX"]
    [(= tens 8) "LXXX"]
    [(= tens 9) "XC"]
    [else ""]
    )
  )

(define (ones-to-roman ones)
  (cond
    [(= ones 1) "I"]
    [(= ones 2) "II"]
    [(= ones 3) "III"]
    [(= ones 4) "IV"]
    [(= ones 5) "V"]
    [(= ones 6) "VI"]
    [(= ones 7) "VII"]
    [(= ones 8) "VIII"]
    [(= ones 9) "IX"]
    [else ""]
    )
  )

(define (nikolay-challenge arabic)
  (string-append (thousands-to-roman (quotient arabic 1000))
                 (hundreds-to-roman (remainder (quotient arabic 100) 10))
                 (tens-to-roman (remainder (quotient arabic 10) 10))
                 (ones-to-roman (remainder arabic 10)))
  )

(equal? (nikolay-challenge 15) "XV")
(equal? (nikolay-challenge 200) "CC")
(equal? (nikolay-challenge 1999) "MCMXCIX")