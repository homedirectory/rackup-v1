#lang racket/base

(require rash/prompt-helpers/string-style)

(provide debug info warn)

(define (bold-color text color)
  (create-styled-string text #:fg color #:bold? #t))

(define (display-log lvl fmt args)
  (displayln (format "[~a] ~a" lvl (apply format (cons fmt args))))
  )

(define-syntax debug
  (syntax-rules ()
    [(debug fmt arg args ...) (debug-log fmt arg args ...)]
    [(debug arg) (debug-log "~a" arg)]))
(define-syntax info
  (syntax-rules ()
    [(info fmt arg args ...) (info-log fmt arg args ...)]
    [(info arg) (info-log "~a" arg)]))
(define-syntax warn
  (syntax-rules ()
    [(warn fmt arg args ...) (warn-log fmt arg args ...)]
    [(warn arg) (warn-log "~a" arg)]))

; generate macros for different logging levels
; TODO how to transform id into id-log
;(define-syntax-rule
;  (make-log-macro id)
;  (define-syntax id
;    (syntax-rules ()
;      [(id fmt arg args (... ...)) (debug-log fmt arg args (... ...))]
;      [(id arg) (debug-log "~a" arg)])))

(define (debug-log fmt . args)
  (display-log (bold-color "DEBUG" "blue") fmt args))

(define (info-log fmt . args)
  (display-log (bold-color "INFO " "white") fmt args))

(define (warn-log fmt . args)
  (display-log (bold-color "WARN " "yellow") fmt args))
