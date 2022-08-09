#lang racket/base

(require rackunit rackunit/text-ui
         racket/stream racket/list)
(require "../helpers.rkt")

(define (double x) (* x 2))

(define helpers-tests 
  (test-suite
    "Tests for helpers.rkt"
    (test-case 
      "recmap"
      (check-equal? 
        (recmap double (list 1 2 (list 3 4 (list 5 6))))
        (list 2 4 (list 6 8 (list 10 12))))
      )
    (test-case
      "stream-reverse"
      (check-equal?
        (stream->list (stream-reverse (stream 1 2 3 4)))
        (list 4 3 2 1))
      )
    (test-case
      "stream-flatmap"
      (check-equal?
        (stream->list 
          (stream-flatmap double (stream 1 2 (stream 3 (stream 4)) 5)))
        (list 2 4 6 8 10))
      )
    (test-case
      "stream-recmap"
      (check-equal?
        (stream->list 
          (stream-flatten 
            (stream-recmap double (stream 1 2 (stream 3 (stream 4 5)) 6))))
        (flatten (list 2 4 (list 6 (list 8 10)) 12)))
      )
    (test-case
      "stream->list!"
      (check-equal?
        (stream->list! (stream 1 2 (stream 3 (stream 4 5)) 6))
        (list 1 2 (list 3 (list 4 5)) 6))
      )
    )
  )

;; ----- RUN TESTS -----
(run-tests helpers-tests)
