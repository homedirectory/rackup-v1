#lang rash

(require racket/bool racket/promise racket/string)
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
(define (mk-bak-file path/str #:encrypt? [encrypt? #f]
                     #:temp? [temp? #f])
  ;(debug "mk-bak-file: ~a" path/str) 
  (let ([path (expand-user-path path/str)])
    (bak-file path encrypt? #f))
  )
(define (bak-file-from-file fil)
  (mk-bak-file (file-path fil)))

(define (bak-file->string fil)
  (format "{~a~a~a}" (path->string (file-path fil))
          (if (bak-file-encrypt? fil) ",ENCRYPT" "")
          (if (bak-file-temp? fil) ",TEMP" "")))

; --- mbf (mem-bak-file) structure ---
; for in-memory contents that need to be backed up
; tmp-dir-path : path?
(struct mbf bak-file (data-delayed))
; mbf constructor
; data : promise?
(define (mk-mbf name data)
    (mbf name #f #t data))

(define (get-mbf-data fil)
  (force (mbf-data-delayed fil)))

(define (touch-mbf fil)
  {
  echo (get-mbf-data fil) &>! (path->string (file-path fil))
  }
  fil
  )


; --- general file procedures ---
(define (encrypt-file fil)
  ;{
  ;    gpg --batch --yes -c -o $(mktemp) (file-path fil)
  ;}
  (warn "encryption is not supported")
  fil
  )


