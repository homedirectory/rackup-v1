#lang racket

(require rackunit rackunit/text-ui)
(require "../backup.rkt"
         "../helpers.rkt"
         )

(define (p str) (string->path str))

(define backup-tests 
  (test-suite
    "Tests for backup.rkt"
    (test-case 
      "indir"
      (for-each
        (lambda (expv actv)
          (cond [(file? expv) (check-equal? (file-path expv) (file-path actv))]
                [(path? expv) (check-equal? expv actv)]))
        (stream->list! 
          (in-dir "lvl1"
                  "a" (f "b")
                  (in-dir "lvl2" "y" (f "z"))))
        (list (p "lvl1/a") (f "lvl1/b") (list (p "lvl1/lvl2/y") (f "lvl1/lvl2/z"))))
      )
    ))




;; ----- RUN TESTS -----
(run-tests backup-tests)

