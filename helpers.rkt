#lang racket/base

(require racket/stream racket/function racket/string)

(provide (except-out (all-defined-out)
                     s-flatmap
                     ))

; recursive map
(define (recmap f lst)
  (map (lambda (x) 
         (if (list? x) (recmap f x)
           (f x)))
       lst))

(define (stream-reverse s)
  (define (iter s1 res)
    (if (stream-empty? s1)
      res
      (iter (stream-rest s1) (stream-cons (stream-first s1) res))
      ))
  (iter s empty-stream))

; TODO make it lazy
(define (s-flatmap f s)
  (define (iter s1 res)
    (if (stream-empty? s1) 
      res
      (let ([x (stream-first s1)])
        (if (stream? x) 
          (iter (stream-rest s1) (stream-append (iter x empty-stream) res)) 
          (iter (stream-rest s1) (stream-cons (f x) res))))
      ))
  (stream-reverse (iter s empty-stream)))
(define-syntax-rule (stream-flatmap f s) (stream-lazy (s-flatmap f s)))

(define (stream-flatten s)
  (stream-flatmap identity s))

(define (stream-recmap f s)
  (stream-map (lambda (x)
                (if (stream? x)
                  (stream-recmap f x)
                  (f x)))
              s))

; applies stream->list to s and nested streams
(define (stream->list! s)
  (stream->list 
    (stream-map (lambda (x)
                  (if (stream? x)
                    (stream->list! x)
                    x))
                s)
    ))

(define (stream-rec-for-each f s)
  (stream-for-each (lambda (x)
                     (if (stream? x) 
                       (stream-rec-for-each f x)
                       (f x)))
                   s))

(define (string-empty? str) 
  (not (non-empty-string? (string-trim str))))
