#lang racket

; Create a delayed expression that, when forced, will evaluate (* 2 3)
(delay (* 2 3))

; Force the evaluation of the delayed expression created above
(force (delay (* 2 3)))

; Define an empty stream as an empty list
(define empty-stream '())

; Define a macro `cons-stream` that constructs a stream with head `h` and tail `t`
; The tail is delayed to achieve lazy evaluation
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t) (cons h (delay t)))
  ))

; Create a stream `s` that contains the elements 1, 2, 3, and then ends with `empty-stream`
(define s (cons-stream 1 (cons-stream 2 (cons-stream 3 empty-stream))))
s ; Displays the created stream `s`

; Define a function `repeat` that creates an infinite stream of a given value `v`
; Each tail in this stream is delayed to achieve lazy, infinite evaluation
(define (repeat v)
  (cons-stream v (repeat v)))

(repeat 5) ; Creates an infinite stream of 5s but does not print or limit it

; Define `head` as an alias for `car`, which extracts the first element of a stream
(define head car)

; Define `tail` as a function that forces the evaluation of the delayed cdr (tail) of a stream
; This function moves one element forward in the stream
(define (tail s)
  (force (cdr s)))

; Get the head (first element) of the infinite stream `(repeat 5)`, which is `5`
(head (repeat 5))

; Define a function `stream-take` that takes the first `n` elements from a given stream
; If `n` is zero or the stream is empty, it returns an empty list
; Otherwise, it cons the head of the stream with the result of taking the next `n-1` elements from the tail
(define (stream-take n stream)
  (if (or (zero? n) (empty? stream))
      '()
      (cons (head stream) (stream-take (sub1 n) (tail stream)))))

; Take the first 5 elements of the infinite stream `(repeat 5)`, which will yield `(5 5 5 5 5)`
(stream-take 5 (repeat 5))
