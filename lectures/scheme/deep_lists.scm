(define dl '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))

(define (atom? x) (and (not (null? x)) (not (pair? x))))

(define (count-atoms dl)
  (cond ((null? dl) 0)
        ((atom? dl) 1)
        (else (+ (count-atoms (car dl)) (count-atoms (cdr dl))))))

(define (count-atoms dl)
  (deep-foldr 0 (lambda (x) 1) + dl))

(define (flatten dl)
  (cond ((null? dl) '())
        ((atom? dl) (list dl))
        (else (append (flatten (car dl)) (flatten (cdr dl))))))

(define (flatten dl)
  (deep-foldr '() list append dl))

(define (snoc x l) (append l (list x)))

(define (deep-reverse dl)
  (cond ((null? dl) '())
        ((atom? dl) dl)
        (else (snoc (deep-reverse (car dl)) (deep-reverse (cdr dl))))))

(define (id x) x)

(define (deep-reverse dl)
  (deep-foldr '() id snoc dl))

(define (deep-foldr nv term op dl)
  (cond ((null? dl) nv)
        ((atom? dl) (term dl))
        (else (op (deep-foldr nv term op (car dl)) (deep-foldr nv term op (cdr dl))))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (deep-foldr nv term op dl)
  (foldr op nv (map (lambda (x) (if (atom? x) (term x) (deep-foldr nv term op x))) dl)))

