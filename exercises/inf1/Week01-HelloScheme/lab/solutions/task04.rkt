#lang racket

(define (round-three-dig n) (/ (floor (* n 1000)) 1000.0))

(= (round-three-dig 12.345678) 12.345)