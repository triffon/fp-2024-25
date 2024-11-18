#lang racket

(define output-port (open-output-file "output.txt"))

(define content "This is some content that will be written to the file.\nAnother line here.")

(display content output-port)

(close-output-port output-port)
