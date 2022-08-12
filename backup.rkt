#!/usr/bin/env racket
#lang rash

(require racket/cmdline racket/list racket/string racket/bool
         racket/promise racket/path racket/function)
(require linea/line-macro)
(require shell/pipeline)
(require "helpers.rkt"
         "macros.rkt"
         "logging.rkt"
         "file-struct.rkt"
         )

(provide (all-defined-out))


; --- main backup logic ---
(define (simulate path files)
  (define (sim-process-file fil)
    (let ([fpath (file-path fil)])
      ;(debug "Processing: ~a" fpath)
      (if (not (can-read? fil))
        (warn "can't read file: ~a" fpath)
        (begin (info "archived: ~a" fpath) #t)
        ;(cond [(bak-file-encrypt? fil) (debug "~a - encrypt" fpath)]
        ;      [else (debug "~a" fpath)])
        )))

  (printf "Simulating backup: ~a\n\n" path)
  ; we want to store all mbf's nicely in one directory, so create a temporary one for that
  (let* ([mbf-tmp-dir (string-append #{mktemp -u} "_stdout")] 
         [files (map (lambda (x)
                       (when (mbf? x) 
                         (set-file-path! x (my-build-path mbf-tmp-dir (file-path x))))
                         x)
                       files)])
    (debug "files:\n~a" (string-join (map bak-file->string files) "\n"))
    (for-each sim-process-file files))
  (printf "\nDone\n")
  )

; path: path?
; files: (listof file?)
; -> (or/c file? #f)
; returns #f if no files were given
(define (mk-backup path files)
  ; path1 : path?
  ; files1 : (listof file?)
  ; -> (or/c file? #f)
  (define (archive-files path1 files1)
    ; --- internal definitions ---
    (define (pre-process-file fil)
      (cond [(mbf? fil) (touch-mbf fil)]
            [else fil])
      )
    (define (archive-file fil)
      (let ([fpath (file-path fil)])
        ; order of cond predicates matters since we have a structure hierarchy
        (cond [(bak-file? fil) 
               { tar -rPh -f (path->string path1) (path->string fpath) }]
              )
        fil))
    (define (post-process-file fil)
      (cond [(bak-file-temp? fil) 
             { rm -r (path->string (file-path fil)) }])
      fil)
    ; --- END internal definitions ---

    (if (empty? files1)
      #f
      (begin
        ; archive files into tar
        (for-each 
          (lambda (fil)
            (let* ([fil (pre-process-file fil)]
                   [fpath (file-path fil)])
              (with-handlers 
                ([exn:fail? (lambda (e) 
                              (warn "can't backup: ~a" fpath))])
                (archive-file fil)
                (info "archived: ~a" fpath))
              (post-process-file fil)))
          files1)
        (file path1))))

  (printf "Creating backup: ~a\n\n" path) 
  ;{ touch (path->string path) } ; tar -rf will create a tar file if it doesn't exist yet

  ; we want to store all mbf's nicely in one directory, so prepare (dry run) a temporary one for that
  (let* ([mbf-tmp-dir (mkdir (string-append #{mktemp -u} "_stdout"))] 
         [files (map (lambda (x)
                       (when (mbf? x) 
                         (set-file-path! x (my-build-path mbf-tmp-dir (file-path x))))
                       x)
                     files)])
    (debug "files:\n~a" (string-join (map bak-file->string files) "\n"))
    (let* ([files-to-encrypt (filter bak-file-encrypt? files)]
           [archive-encrypted 
             ; 1. archive files to be encrypted in a single tar
             ; no need to compress, since encryption does it
             ; archive-files returns #f if no files were given
             (let ([archive-to-be-encrypted 
                     (archive-files (string->path (string-append #{mktemp -u} "_encrypted"))
                                    files-to-encrypt)])
               (if archive-to-be-encrypted
                 ; 2. encrypt the archive from 1.
                 ; res : file?
                 (let ([res (encrypt-file archive-to-be-encrypted)])
                   (if res
                     (begin
                       ; remove the original archive
                       #{rm -f (file-path archive-to-be-encrypted)}
                       ; mark the encrypted archive as a temporary file
                       (struct-copy bak-file (bak-file-from-file res) [temp? #t]))
                     #f))
                 #f)
               )]
           [files-rest (filter (negate bak-file-encrypt?) files)])
      (when archive-encrypted
        (debug "encrypted archive: ~a" (file-path archive-encrypted)))
      ; 3. include the encrypted archive from 2. into the final backup
      (let ([final-backup-file
              (compress-file 
                (archive-files path (if archive-encrypted
                                      (cons archive-encrypted files-rest)
                                      files-rest)))])
        (newline)
        (info "Backup ready -> ~a" (file-path final-backup-file)))
      ))
  )

