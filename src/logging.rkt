#lang racket/base

(provide debug info warn)

(define colors-table
  #hash(("black" . 30)
        ("red" . 31)
        ("green" . 32)
        ("yellow" . 33)
        ("blue" . 34)
        ("magenta" . 35)
        ("cyan" . 36)
        ("white" . 37)))

(define (style-string text color #:bold? [bold? #f])
  (let ((bold (if bold? "1;" ""))
        (color-code (hash-ref colors-table color)))
    (format "\033[~a~am~a\033[0m" bold color-code text)))

(define (bold-color text color)
  (style-string text color #:bold? #t))

(define (display-log lvl fmt args)
  (display (format "[~a] " lvl))
  (displayln (apply format (cons fmt args)))
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
