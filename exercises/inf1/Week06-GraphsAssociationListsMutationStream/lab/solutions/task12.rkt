#lang racket

(define (display-file-content path)
  (define input-port (open-input-file path))
  (define (helper current-line)
    (if (eof-object? current-line)
        (begin
          (close-input-port input-port)
          (displayln "<EOF>")
          )
        (begin
          (displayln current-line)
          (helper (read-line input-port))
          ))
    )
  (helper (read-line input-port))
  )

(display-file-content "task02.rkt")