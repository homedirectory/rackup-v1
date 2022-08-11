#lang racket/base

(require racket/stream racket/function racket/string
         racket/path racket/list)

(provide (all-defined-out))

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

(define (stream-flatmap f s)
  (define (iter s1 res)
    (if (stream-empty? s1) 
      res
      (let ([x (stream-first s1)])
        (if (stream? x) 
          (iter (stream-rest s1) (stream-append (iter x empty-stream) res)) 
          (iter (stream-rest s1) (stream-cons (f x) res))))
      ))
  (stream-reverse (iter s empty-stream)))
;(define-syntax-rule (stream-flatmap f s) (stream-lazy (s-flatmap f s)))

(define (stream-flatten s)
  (stream-flatmap identity s))

; recursively map nested streams
(define (stream-recmap f s)
  (stream-map (lambda (x)
                (if (stream? x)
                  (stream-recmap f x)
                  (f x)))
              s))

; like stream-recmap, but don't recurse
(define (stream-norecmap f s)
  (stream-map (lambda (x)
                (if (stream? x)
                  x
                  (f x)))
              s))

; recursively filter elements of stream
(define (stream-recfilter pred s)
  (define (iter s1 res)
    (if (stream-empty? s1) 
      res
      (let ([x (stream-first s1)])
        (if (stream? x) 
          (iter (stream-rest s1) (if (stream-empty? res) 
                                   (iter x empty-stream) 
                                   (stream-append res (stream (iter x empty-stream))))) 
          (iter (stream-rest s1) (if (pred x) (stream-append res (stream x)) res))))
      ))
  (stream-lazy (iter s empty-stream)))

; like stream-recfilter, but don't recurse
(define (stream-norecfilter pred s)
  (define (iter s1 res)
    (if (stream-empty? s1) 
      res
      (let ([x (stream-first s1)])
        (if (stream? x) 
          (iter (stream-rest s1) (if (stream-empty? res) 
                                   x 
                                   (stream-append res (stream x)))) 
          (iter (stream-rest s1) (if (pred x) (stream-append res (stream x)) res))))
      ))
  (stream-lazy (iter s empty-stream)))


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

; alt if val is #f else val
(define (val-or val alt)
  (if val val alt))

; alt if val is #f else (f val)
(define (val-map-or f val alt)
  (if val (f val) alt))

(define (my-build-path base sub . subs)
  (let ([args (map 
                (lambda (x) (if (string? x) (string->path x) x))
                (filter 
                  (negate (lambda (x) (and (string? x) (string-empty? x))))
                  (append (list base sub) subs)))])
    (if (empty? args)
      ""
      (apply build-path args))))

(define (my-path-get-extension path-string)
  (val-map-or bytes->string/utf-8 (path-get-extension path-string) ""))
