(define (at lst n)
  (cond ((null? lst) "undefined")
        ((zero? n) (car lst))
        (else (at (cdr lst)
                  (- n 1)))))

(define (take-n n lst)
  (if (or (null? lst)
          (zero? n)) '()
      (cons (car lst)
            (take-n (- n 1)
                    (cdr lst)))))

(define (take-n-iter n lst)
  (define (for index result lst)
    (if (or (null? lst)
            (zero? index)) result
        (for (- index 1)
             (append result (list (car lst)))
             (cdr lst))))
  (for n '() lst))

(define (drop-n n lst)
  (if (or (null? lst)
          (zero? n)) lst
      (drop-n (- n 1) (cdr lst))))

(define (sorted? lst)
  (cond ((or (null? lst)
             (null? (cdr lst))) #t)
        ((> (car lst)
            (cadr lst)) #f)
        (else (sorted? (cdr lst)))))
      
(define (zip lst1 lst2)
  (if (or (null? lst1)
          (null? lst2)) '()
      (cons (cons (car lst1)
                  (car lst2))
            (zip (cdr lst1)
                 (cdr lst2)))))

(define (max-repeated lst)
  (define (helper prev size max-size lst)
    (cond ((null? lst)
           (max size max-size))
          ((equal? prev (car lst))
           (helper (car lst)
                   (+ size 1)
                   max-size
                   (cdr lst)))
          (else (helper (car lst)
                        1
                        (max size max-size)
                        (cdr lst)))))
  (if (null? lst) 0
      (helper (car lst) 1 1 (cdr lst))))

(define (foldr operation null_value lst)
  (if (null? lst) null_value
      (operation (car lst)
          (foldr operation null_value (cdr lst)))))

(define (foldl operation null_value lst)
  (if (null? lst) null_value
      (foldl operation (operation null_value (car lst)) (cdr lst))))

(define (map* f lst)
  (foldr (lambda (current result)
           (cons (f current) result))
         '()
         lst))

(define (filter predicate? lst)
  (foldr (lambda (current result)
           (if (predicate? current)
               (cons current result)
               result)) '() lst))

(define (all? predicate? lst)
  (foldl (lambda (result current)
           (and result (predicate? current)))
         #t lst))

(define (any? predicate? lst)
  (foldl (lambda (result current)
           (or result (predicate? current)))
         #f lst))

(define (member? x lst)
  (any? (lambda (y) (equal? x y)) lst))

(define (unique lst)
  (foldr (lambda (current result)
           (if (member? current result)
               result
               (cons current result)))
         '() lst))

(define (union lst1 lst2)
  (unique (append lst1 lst2)))

(define (intersection lst1 lst2)
  (filter (lambda (x) (member? x lst2)) lst1))

(define (flip f)
  (lambda (x y)
    (f y x)))

(define (reverse* lst)
  (foldl (flip cons) '() lst))

(define (tupled f)
  (lambda (x)
    (f (car x)
       (cdr x))))

(define (zipWith f lst1 lst2)
  (map (tupled f) (zip lst1 lst2)))

(define (quick-sort lst)
  (if (null? lst) '()
      (let ((pivot (car lst))
            (rest (cdr lst)))
        (append (quick-sort (filter (lambda (x) (< x pivot)) rest))
                (list pivot)
                (quick-sort (filter (lambda (x) (>= x pivot)) rest))))))



 