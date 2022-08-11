#lang racket

(require rackunit rackunit/text-ui)
(require "../helpers.rkt"
         "../file-struct.rkt"
         "../interface.rkt"
         )

(define (p str) (string->path str))

(define interface-tests
  (test-suite
    "Tests for interface.rkt"
    (test-case 
      "in-dir simple"
      (for-each
        (lambda (expv actv)
          (check-equal? (file-path expv) (file-path actv)))
        (stream->list! 
          (in-dir "/etc" "sudoers"))
        (list (f "/etc/sudoers"))
        ))
    (test-case
      "in-dir-stream is lazy"
      (define num 0)
      (define (incnum) (set! num (+ 1 num)))
      (define (f x) (incnum) x)
      (define (files) 
        (set! num 0)
        (in-dir-stream "/root"
                       (stream (f "a")
                               (in-dir-stream "tmp"
                                              (stream (f "b")))
                               (f "c"))))
      ;(stream-flatten (files))
      (files)
      (check-equal? num 0))
    (test-case 
      "in-dir"
      (for-each
        (lambda (expv actv)
          (cond [(file? expv) (check-equal? (file-path expv) (file-path actv))]
                [(path? expv) (check-equal? expv actv)]))
        (stream->list! 
          (in-dir "lvl1"
                  "a" (f "b")
                  (in-dir "lvl2" "y" (f "z"))))
        (list (f "lvl1/a") (f "lvl1/b") (list (f "lvl1/lvl2/y") (f "lvl1/lvl2/z"))))
      )))

; --- RUN TESTS ---
(run-tests interface-tests)
