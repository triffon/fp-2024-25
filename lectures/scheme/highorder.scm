(define fact (lambda (n)
               (if (= n 0) 1 (* n (fact (- n 1))))))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (id x) x)
(define (1+ x) (+ x 1))

(define (fact n)
  (accumulate * 1 1 n (lambda (i) i) 1+))

(define (pow x n)
  (accumulate * 1 1 n (lambda (i) x) 1+))

(define (myexp x n)
  (accumulate + 0 0 n (lambda (i) (/ (pow x i) (fact i))) 1+))

(define (myexp x n)
  (accumulate + 0 0 n (lambda (i) (/ (accumulate * 1 1 i (lambda (i) x) 1+)
                                     (accumulate * 1 1 i (lambda (i) i) 1+))) 1+))

(define (exists? a b p?)
  (accumulate (lambda (x y) (or x y)) #f a b p? 1+))

(define (square x) (* x x))
(define (twice f x) (f (f x)))
(define (twice f) (lambda (x) (f (f x))))

(define (n+ n)
  (lambda (i) (+ i n)))

(define 1+ (n+ 1))
(define 5+ (n+ 5))

(define (derive f dx)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (repeated f n)
  (lambda (x)
    (if (= n 0) x
        (f ((repeated f (- n 1)) x)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0) id
      (compose f (repeated f (- n 1)))))

;; (define (accumulate op nv a b term next)

(define (repeated f n)
  (accumulate compose id 1 n (lambda (i) f) 1+))

(define (derive-n f n dx)
  (if (= n 0) f
      (derive (derive-n f (- n 1) dx) dx)))

(define (derive-n f n dx)
  ((repeated (lambda (f) (derive f dx)) n) f))

(define (derive_ dx)
  (lambda (f)
     (lambda (x) (/ (- (f (+ x dx)) (f x)) dx))))

(define (derive-n_ n dx)
     (repeated (lambda (f) (derive f dx)) n))

(define my-#t (lambda (x y) x))
(define my-#f (lambda (x y) y))
(define (my-if b x y) ((b x y)))

(define (exists? p? l)
  (cond ((null? l) #f)
        ((p? (car l)) #t)
        (else (exists? p? (cdr l)))))

(define (exists? p? l)
  (and (not (null? l)) (or (p? (car l)) (exists? p? (cdr l)))))

(define (member? x l)
  (exists? (lambda (y) (equal? x y)) l))

(define (search p l)
  (and (not (null? l)) (or (p (car l)) (search p (cdr l)))))

(define (member? x l)
  (search (lambda (y) (equal? x y)) l))

(define (assoc key alist)
  (search (lambda (kv) (and (equal? (car kv) key) kv)) alist))

(define (all? p? l)
  (null? (filter (lambda (x) (not (p? x))) l)))

(define (all? p? l)
  (not (search (lambda (x) (not (p? x))) l)))