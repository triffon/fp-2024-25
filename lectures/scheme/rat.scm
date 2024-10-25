(define (make-rat n d)
  (if (= d 0)
      (cons n 1)
      (if (< d 0) (make-rat (- n) (- d))
          (let* ((g (gcd n d))
                 (ng (quotient n g))
                 (dg (quotient d g)))
            (cons ng dg)))))

(define get-numer car)
(define get-denom cdr)

(define (*rat p q)
  (make-rat
    (* (get-numer p) (get-numer q))
    (* (get-denom p) (get-denom q))))

(define (+rat p q)
  (make-rat
    (+ (* (get-numer p)
          (get-denom q))
       (* (get-denom p)
          (get-numer q)))
    (* (get-denom p) (get-denom q))))

(define (<rat p q)
  (< (* (get-numer p) (get-denom q))
     (* (get-numer q) (get-denom p))))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (1+ x) (+ x 1))

(define (fact n)
  (accumulate * 1 1 n (lambda (i) i) 1+))

(define (pow x n)
  (accumulate * 1 1 n (lambda (i) x) 1+))

(define (todouble r)
  (+ .0 (/ (get-numer r) (get-denom r))))


(define (myexp x n)
  (accumulate +rat (make-rat 0 1) 0 n (lambda (i)
                                        (make-rat (pow x i) (fact i))) 1+))