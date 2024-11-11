#lang racket

(list '(1 . "One") '(2 . "Two") '(3 . "Three"))

(assoc 1 '((1 . "One") (2 . "Two") (3 . "Three") (1 . "Four")))
(assoc 4 '((1 . "One") (2 . "Two") (3 . "Three") (1 . "Four")))