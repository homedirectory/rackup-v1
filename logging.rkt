#lang racket/base

(require rash/prompt-helpers/string-style)

(provide (except-out (all-defined-out)
                     display-log))

(define (bold-color text color)
  (create-styled-string text #:fg color #:bold? #t))

(define (display-log lvl fmt args)
  (displayln (format "[~a] ~a" lvl (apply format (cons fmt args))))
  )

(define (debug fmt . args)
  (display-log (bold-color "DEBUG" "blue") fmt args))

(define (info fmt . args)
  (display-log (bold-color "INFO " "white") fmt args))

(define (warn fmt . args)
  (display-log (bold-color "WARN " "yellow") fmt args))
