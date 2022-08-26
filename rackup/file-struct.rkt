#lang racket/base

(require racket/bool racket/promise racket/string racket/file racket/system)
(require "macros.rkt" "helpers.rkt" "logging.rkt")

(provide (all-defined-out))

; ---------------------------------------------------------
; file

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

; ---------------------------------------------------------
; bak-file

; for files that are backed up
; path encrypt? temp?
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
  (my-string-join
    (list
      (if (bak-dir? fil) "bak-dir" "")
      (path->string (file-path fil))
      (if (bak-file-encrypt? fil) "ENCRYPT" "")
      (if (bak-file-temp? fil) "TEMP" "")
      (if (and (bak-dir? fil) (bak-dir-exclude? fil))
        (format "exclude:~a" (bak-dir-files fil))
        ""))
    ",")
  )

; ---------------------------------------------------------
; bak-dir

; files : (or/c bak-file? bak-dir? string?)
; path encrypt? temp? exclude? files
(struct bak-dir bak-file (exclude? files))
(define (mk-bak-dir path/str #:exclude? [exclude? #f] #:encrypt? [encrypt? #f] files)
  ; convert all string's in files to bak-file's (if not exclude)
  (let ([files (if exclude?
                 files
                 (map-if string? mk-bak-file files))])
    (bak-dir 
      ; allow empty strings as dir path
      (if (and (string? path/str) (string-blank? path/str)) 
        "" 
        (expand-user-path path/str)) 
      encrypt? #f exclude? files)))

; -> (listof bak-file? bak-dir?)
; if files of obj contain bak-dir, then:
;   if bak-dir has exclude #t, it is kept as is
;   else bak-dir->list is applied
(define (bak-dir->list obj)
  (when (bak-dir-exclude? obj)
    (error "bak-dir->list: bak-dir has exclude? = #t"))
  (let ([dir-path (file-path obj)])
    ;(debug "~s ~s" dir-path (bak-dir-files obj))
    (flatmap (lambda (x)
               (cond [(bak-dir? x) 
                      (let ([x 
                              (struct-copy bak-dir x 
                                           [path #:parent file 
                                                 (my-build-path dir-path (file-path x))])])
                        (if (bak-dir-exclude? x)
                          x
                          (bak-dir->list x)))]
                     [(bak-file? x) 
                      (set-file-path! 
                        x 
                        (my-build-path dir-path (file-path x)))
                      x]
                     [else (error "bak-dir->list: Unexpected file: ~a" x)]))
             ; (listof bak-file? bak-dir?)
             (bak-dir-files obj))))

; ------------------------------------------------------------
; mbf (mem-bak-file)

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
  (display-to-file (get-mbf-data fil) (file-path fil))
  fil)


; ------------------------------------------------------------
; general file procedures

; fil : file?
; -> file?
(define (encrypt-file fil)
  ;(warn "encryption is not supported")
  (let* ([path (file-path fil)]
         [enc-path (path-add-extension path ".enc" ".")])
    (with-handlers ([exn:fail (lambda (e) (warn e) #f)])
      (system (format "gpg -c -o ~s ~s" (path->string enc-path) (path->string path)))
      (file enc-path))))

; fil : file?
; -> file?
(define (compress-file fil)
  (let ([path (file-path fil)])
    (with-handlers ([exn:fail (lambda (e) (warn e) #f)])
      (system (format "gzip -f ~s" (path->string path)))
      (file (path-add-extension path ".gz" ".")))))
