#lang racket/base

(require rackunit rackunit/text-ui
         racket/stream racket/list)
(require "../src/helpers.rkt")

(define (double x) (* x 2))
(define num 0)
(define (incnum) (set! num (+ 1 num)))
(define (f x) (incnum) x)

; producer : procedure?
(define (check-stream-is-lazy? producer)
  (set! num 0)
  (let ([n 0])
    (stream-for-each (lambda (x)
                       (set! n (+ n 1))
                       (check-equal? num n))
                     (producer))))

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
      "stream-reverse is lazy"
      (define (mk-stream) 
        (stream-reverse (stream (f 1) (f 2) (f 432) (f -2))))
      (check-stream-is-lazy? mk-stream))
    ;(test-case
    ; "stream-flatmap is lazy"
    ; (define (mk-stream) 
    ;  (stream-flatmap double 
    ;  (stream (f 1) (f 2) (stream (f -2) (f 48)) (f 432) (f -2))))
    ; (check-stream-is-lazy? mk-stream))
    ;(test-case
    ;  "stream-flatmap"
    ;  (check-equal?
    ;    (stream->list 
    ;      (stream-flatmap double (stream 1 2 (stream 3 (stream 4)) 5)))
    ;    (list 2 4 6 8 10))
    ;  )

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
