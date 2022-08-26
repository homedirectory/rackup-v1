#lang racket

(require rackunit rackunit/text-ui)
(require "../rackup/backup.rkt"
         "../rackup/helpers.rkt"
         "../rackup/file-struct.rkt"
         "../rackup/interface.rkt"
         )

(define backup-tests 
  (test-suite
    "Tests for backup.rkt"
    
    ))




;; ----- RUN TESTS -----
(run-tests backup-tests)

