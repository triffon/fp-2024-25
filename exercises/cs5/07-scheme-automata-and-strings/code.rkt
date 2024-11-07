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
   (cons 'delta  ; associative list mapping pairs of state and symbol to state
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

(define (apply-delta-dumb* delta state charlist)
  (if (null? charlist)
      state
      (apply-delta* delta (apply-delta delta state (car charlist)) (cdr charlist))))

(define (apply-delta* delta state charlist)
  (foldl (lambda (symb new-state) (apply-delta delta new-state symb)) state charlist))

(define (accepts-word? automaton word)
  (let
      ((delta (assoc-get baba-automaton 'delta))
       (start (assoc-get baba-automaton 'start))
       (final (assoc-get baba-automaton 'final))
       (charlist (string->list word)))
    (member? (apply-delta* delta start charlist) final)))