(define (length l)
  (if (null? l) 0
      (+ 1 (length (cdr l)))))

(define (length l)
  (foldr (lambda (x r) (+ 1 r)) 0 l))

(define (append l1 l2)
  (if (null? l1) l2
      (cons (car l1) (append (cdr l1) l2))))

(define (append l1 l2)
  (foldr cons l2 l1))

(define (snoc x l)
  (append l (list x)))

(define (reverse l)
  (if (null? l) l
      (append (reverse (cdr l)) (list (car l)))))

(define (reverse l)
  (if (null? l) l
      (snoc (car l) (reverse (cdr l)))))

(define (reverse l)
  (foldr snoc '() l))

(define (list-tail l n)
  (if (= n 0) l
      (list-tail (cdr l) (- n 1))))

(define (list-ref l n)
  (if (= n 0) (car l)
      (list-ref (cdr l) (- n 1))))

(define (member x l)
  (cond ((null? l) #f)
        ((equal? x (car l)) l)
        (else (member x (cdr l)))))

(define (member* =? x l)
  (cond ((null? l) #f)
        ((=? x (car l)) l)
        (else (member* =? x (cdr l)))))

(define (member* =? x l)
  (and (not (null? l)) (or (and (=? x (car l)) l) (member* =? x (cdr l)))))

;; (define (member* =? y l)
;;  (foldr (lambda (x r) (or (=? x y) r)) #f l))

(define (member x l) (member* equal? x l))
(define (memv x l) (member* eqv? x l))
(define (memq x l) (member* eq? x l))

(define (from-to a b)
  (if (> a b) '()
      (cons a (from-to (+ a 1) b))))

(define (collect a b next)
  (if (> a b) '()
      (cons a (collect (next a) b next))))

(define 1+ (lambda (x) (+ x 1)))

(define (from-to a b)
  (collect a b 1+))


(define (filter p? l)
;;  (map (lambda (x) (if (p? x) x '())) l))
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))

(define (map f l)
  (if (null? l) l
      (cons (f (car l)) (map f (cdr l)))))

(define (map f l)
  (foldr (lambda (x r) (cons (f x) r)) '() l))

(define (accumulate op nv a b term next)
  (foldr op nv (map term (collect a b next))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (filter p? l)
;;  (map (lambda (x) (if (p? x) x '())) l))
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))

(define (filter p? l)
  (foldr (lambda (x r) (if (p? x) (cons x r) r)) '() l))

