#lang rash

(require racket/bool racket/promise)
(require "macros.rkt" "helpers.rkt" "logging.rkt")

(provide (all-defined-out))

; --- file structure ---
; for unix files/directories
; path : path?
(struct file ([path #:mutable]))

; path : path?
(define (file/dir-exists? path)
  (not (false? (file-or-directory-type path #f))))
(define (dir? fil) (eq? 'directory (file-or-directory-type fil #f)))
(define (link? fil) (eq? 'link (file-or-directory-type fil #f)))
(define (can-read? fil)
  (or (mbf? fil) 
      (and (file/dir-exists? (file-path fil)) 
           (not (false? (memq 'read 
                              (file-or-directory-permissions (file-path fil))))))))

; --- bak-file structure ---
; for files that are backed up
(struct bak-file file (encrypt? temp?))
; bak-file constructor
(define (mk-bak-file path/str #:encrypt [encrypt? #f])
  ;(debug "mk-bak-file: ~a" path/str) 
  (let ([path (expand-user-path path/str)])
    (bak-file path encrypt? #f))
  )

; --- mbf (mem-bak-file) structure ---
; for in-memory contents that need to be backed up
; tmp-dir-path : path?
(struct mbf bak-file (data-delayed [tmp-dir-path #:mutable #:auto])
        #:auto-value #f)
; mbf constructor
; data : promise?
(define (mk-mbf name data)
    (mbf (my-build-path "stdout" name) #f #t data))

(define (get-mbf-data obj)
  (force (mbf-data-delayed obj)))

(define (get-mbf-full-path obj)
  (my-build-path (mbf-tmp-dir-path obj) (file-path obj)))

(define (touch-mbf obj)
    (set-mbf-tmp-dir-path! obj (string->path #{ mktemp -d }))
    (let ([mbf-full-path-string (path->string (get-mbf-full-path obj))])
    {
      mkdir -p #{dirname "$mbf-full-path-string"}
      echo (get-mbf-data obj) &>! "$mbf-full-path-string"
    }
    obj
    ))


; --- general file procedures ---
(define (encrypt-file fil)
  ;{
  ;    gpg --batch --yes -c -o $(mktemp) (file-path fil)
  ;}
  (warn "encryption is not supported")
  fil
  )


