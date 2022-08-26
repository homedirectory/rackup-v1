#lang racket/base

(require racket/function racket/string racket/path racket/list)
(require "logging.rkt")

(provide (all-defined-out))

; recursive map
(define (recmap f lst)
  (map (lambda (x) 
         (if (list? x) (recmap f x)
           (f x)))
       lst))

(define (flatmap f lst)
  (flatten (recmap f lst)))

(define (string-blank? str) 
  (not (non-empty-string? (string-trim str))))

; alt if val is #f else val
(define (val-or val alt)
  (if val val alt))

; alt if val is #f else (f val)
(define (val-map-or f val alt)
  (if val (f val) alt))

; if (pred x) -> (f x), else x
(define (map-if pred f lst)
  (map (lambda (x)
         (if (pred x) (f x) x))
       lst))

; strs : list?
(define (my-string-join strs sep)
  (string-join (filter-not string-blank? strs) sep))


; ---------------------------------------------------------
; filesystem

(define (my-build-path base sub . subs)
  ;(debug "my-build-path ~a ~a ~a" base sub subs)
  (let ([args (map 
                (lambda (x) (if (string? x) (string->path x) x))
                (filter 
                  (negate (lambda (x) (and (string? x) (string-blank? x))))
                  (append (list base sub) subs)))])
    (if (empty? args)
      ""
      (apply build-path args))))

(define (my-path-get-extension path-string)
  (val-map-or bytes->string/utf-8 (path-get-extension path-string) ""))

; path : path-string?
; -> path? (that is equal to path)
(define (mkdir path)
  (make-directory path)
  (if (string? path)
    (string->path path)
    path))

; ---------------------------------------------------------

(define letters (string->list "abcdefghijklmnopqrstuvwxyz"))
(define numbers (string->list "0123456789"))
(define rand-string-char-list (append letters (map char-upcase letters) numbers))
(define (rand-string n)
  (let ([len (length rand-string-char-list)])
    (list->string 
      (for/list ([i (in-range n)])
        (list-ref rand-string-char-list (random len))))))

(define (mktemp-path)
  (string->path (string-append "/tmp/tmp." (rand-string 10))))

(define fs-strings (vector-immutable "<1k" "k" "M" "G"))
(define (file-size-string path-string)
  (let* ((size-bytes (file-size path-string))
         (vec-ind (min 
                    (inexact->exact (floor (log size-bytes 1024)))
                    (- (vector-length fs-strings) 1))))
    (if (= 0 vec-ind)
      "<1k"
      (format "~a~a" 
              (quotient size-bytes (expt 1024 vec-ind)) 
              (vector-ref fs-strings vec-ind)))))

(define (fix-date-cmd-arg arg)
  (if (string-prefix? arg "+") 
    arg 
    (string-append "+" arg)))
