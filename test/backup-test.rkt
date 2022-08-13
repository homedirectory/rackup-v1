#lang racket

(require rackunit rackunit/text-ui)
(require "../src/backup.rkt"
         "../src/helpers.rkt"
         "../src/file-struct.rkt"
         "../src/interface.rkt"
         )

(define backup-tests 
  (test-suite
    "Tests for backup.rkt"
    
    ))




;; ----- RUN TESTS -----
(run-tests backup-tests)

