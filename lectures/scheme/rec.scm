(define (fact n)
  (if (= n 0) 1
      (* n (fact (- n 1)))))

(define fact
  (lambda (n)
    (if (= n 0) 1
      (* n (fact (- n 1))))))

(define (solve-rec me gamma) (lambda (n) ((gamma (me me gamma)) n)))

(define (id x) x)

(define (gamma f)
  (lambda (n)
    (if (= n 0) 1
      (* n (f (- n 1))))))

;; f = ?, такова че ((solve-rec gamma) f) = f

(define find-fixpoint
  (lambda (gamma)
     (let ((solve-rec (lambda (me) (lambda (n) ((gamma (me me)) n)))))
       (solve-rec solve-rec))))

(define find-fixpoint
  (lambda (gamma)
    ((lambda (me) (lambda (n) ((gamma (me me)) n)))
     (lambda (me) (lambda (n) ((gamma (me me)) n))))))


(define fact (find-fixpoint gamma))

(define (gamma2 f)
  (lambda (n)
    (if (= n 0) 1
      (* 2 (f (- n 1))))))

(define pow2 (find-fixpoint gamma2))
