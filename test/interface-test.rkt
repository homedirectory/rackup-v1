#lang racket

(require rackunit rackunit/text-ui)
(require "../src/helpers.rkt"
         "../src/file-struct.rkt"
         "../src/interface.rkt"
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
        (bak-dir->list (in-dir "/etc" "sudoers"))
        (list (f "/etc/sudoers"))
        ))
    ;(test-case
    ;  "in-dir-stream is lazy"
    ;  (define num 0)
    ;  (define (incnum) (set! num (+ 1 num)))
    ;  (define (f x) (incnum) x)
    ;  (define (files) 
    ;    (set! num 0)
    ;    (in-dir-stream "/root"
    ;                   (stream (f "a")
    ;                           (in-dir-stream "tmp"
    ;                                          (stream (f "b")))
    ;                           (f "c"))))
    ;  ;(stream-flatten (files))
    ;  (files)
    ;  (check-equal? num 0))
    (test-case 
      "in-dir"
      (for-each
        (lambda (expv actv)
          (check-equal? (file-path expv) (file-path actv)))
        (bak-dir->list (in-dir "lvl1"
                               "a" (f "b")
                               (in-dir "lvl2" "y" (f "z"))))
        (list (f "lvl1/a") (f "lvl1/b") (f "lvl1/lvl2/y") (f "lvl1/lvl2/z")))
      )
    (test-case 
      "in-dir with exclude"
      (for-each
        (lambda (expv actv)
          (cond [(bak-file? expv) (check-equal? (file-path expv) (file-path actv))]
                [(bak-dir? expv) (check-equal? (bak-dir-files expv) (bak-dir-files actv))]))
        (bak-dir->list (in-dir "lvl1"
                               "a" (f "b")
                               (in-dir "lvl2" #:exclude? #t "y" "z")))
        (list (f "lvl1/a") (f "lvl1/b") (mk-bak-dir "lvl1/lvl2" #:exclude? #t (list "y" "z"))))
      )
    (test-case 
      "files macro"
      (for-each
        (lambda (expv actv)
          (check-equal? (file-path expv) (file-path actv)))
        (bak-dir->list (files 
                         "root" 
                         (in-dir "lvl1"
                                 "a" (f "b")
                                 (in-dir "lvl2" "y" (f "z")))))
        (list (f "root") (f "lvl1/a") (f "lvl1/b") (f "lvl1/lvl2/y") (f "lvl1/lvl2/z")))
      )
    (test-case 
      "bak-dir->list always returns (listof file? bak-dir?)"
      ; that is, always flat, even if excludes are nested
      (for-each
        (lambda (expv actv)
          (check-equal? (file-path expv) (file-path actv)))
        (bak-dir->list (files 
                         "root" 
                         (in-dir "lvl1" "a"
                                 (in-dir "lvl2" "b"
                                         (in-dir "lvl3" #:exclude? #t "c")))))
        (list (f "root") (f "lvl1/a") (f "lvl1/lvl2/b") 
              (mk-bak-dir "lvl1/lvl2/lvl3" #:exclude? #t (list "c"))))
      )
    ))

; --- RUN TESTS ---
(run-tests interface-tests)
