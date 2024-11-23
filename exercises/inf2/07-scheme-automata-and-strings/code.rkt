#lang racket


(define (exists? p l)
  (if (null? l)
      #f
      (or (p (car l))
          (exists? p (cdr l)))))


(define (member? x l)
  (exists? (lambda (y) (equal? x y)) l))

(define assoc-empty
  '())

(define (assoc-empty? l)
  (null? l))

(define (assoc-set l k v)
  (define (update-kv pair)
    (if (equal? (car pair) k)
        (cons k v)
        pair))
    
  (if (exists? (lambda (pair) (equal? (car pair) k)) l)
      (map (lambda (pair) (update-kv pair)) l)
      (cons (cons k v) l)))


(define (assoc-get l k)
  (if (null? l)
      #f
      (if (equal? (caar l) k)
          (cdar l)
          (assoc-get (cdr l) k))))


(define baba-automaton
  (list
   (cons 'delta
         (list
            (cons (cons 'gosho #\b) 'pesho)
            (cons (cons 'gosho #\a) 'err)
            (cons (cons 'pesho #\b) 'err)
            (cons (cons 'pesho #\a) 'gosho)
            (cons (cons 'err #\a) 'err)
            (cons (cons 'err #\b) 'err)))

   (cons 'start 'gosho)
   (cons 'final (list 'gosho))))

(define (apply-delta delta state symb)
  (assoc-get delta (cons state symb)))

(define (apply-delta-dumb* delta state word)
  (if (null? word)
      state
      (apply-delta* delta
                    (apply-delta delta state (car word))
                    (cdr word))))

(define (apply-delta* delta state word)
  (foldl (lambda (symb state) (apply-delta delta state symb)) state word))

(define (accepts-word? automaton word)
  (member?
   (apply-delta*
    (assoc-get automaton 'delta)
    (assoc-get automaton 'start)
    (string->list word))
   (assoc-get automaton 'final)))