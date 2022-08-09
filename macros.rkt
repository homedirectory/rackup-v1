#lang racket/base

(provide (all-defined-out))

(define-syntax-rule 
  (alias-proc alias-id proc-id)
  (define-syntax-rule (alias-id args (... ...)) (proc-id args (... ...))))

;(define-syntax-rule
;  (stream-of-args proc macro-id)
;  (define-syntax-rule
;    (macro-id args...)
;    (proc (stream args...))
;    ))
