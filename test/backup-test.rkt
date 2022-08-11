#lang racket

(require rackunit rackunit/text-ui)
(require "../backup.rkt"
         "../helpers.rkt"
         "../file-struct.rkt"
         "../interface.rkt"
         )

(define backup-tests 
  (test-suite
    "Tests for backup.rkt"
    
    ))




;; ----- RUN TESTS -----
(run-tests backup-tests)

