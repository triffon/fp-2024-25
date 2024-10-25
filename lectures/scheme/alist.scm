(define (from-to a b)
  (if (> a b) '()
      (cons a (from-to (+ a 1) b))))

(define (1+ x) (+ x 1))

(define (make-alist f keys)
  (map (lambda (key) (cons key (f key))) keys))

(define (keys alist)
  (map car alist))

(define (values alist)
  (map cdr alist))


(define (filter p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))

(define (del-assoc alist key)
  (filter (lambda (kv) (not (eqv? (car kv) key))) alist))

(define al (make-alist 1+ (from-to 1 10)))

;; dict[key] = value

(define (add-assoc alist key value)
  (cons (cons key value) (del-assoc alist key)))