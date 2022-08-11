#!/usr/bin/env racket
#lang rash

(require racket/cmdline racket/list racket/stream racket/string racket/bool
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
        (cond [(bak-file-encrypt? fil) (debug "~a - encrypt" fpath)]
              [(mbf? fil) (debug "mbf: ~a" (my-build-path #{ mktemp -u } fpath))]
              [else (debug "~a" fpath)]))))

  (printf "Simulating backup: ~a\n\n" path)
  (stream-for-each sim-process-file files)
  (printf "\nDone\n")
  )

; path: path?
; files: (streamof file?)
; -> (or/c file? #f)
(define (archive path files)
  ; path1 : path?
  ; files1 : (streamof file?)
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
        (cond [(mbf? fil) 
               { tar -C (path->string (mbf-tmp-dir-path fil)) -rPh -f (path->string path1) (path->string fpath) }]
              [(bak-file? fil) 
               { tar -rPh -f (path->string path1) (path->string fpath) }]
              )
        fil))
    (define (post-process-file fil)
      (cond [(bak-file-temp? fil) 
             (cond [(mbf? fil) 
                    { rm -r (path->string (mbf-tmp-dir-path fil)) }]
                   [else 
                     { rm -r (path->string (file-path fil)) }])]
            )
      fil)
    ; --- END internal definitions ---

    (stream-for-each 
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
    (with-handlers ([exn:fail (lambda (e) (warn e) #f)])
      { gzip -f (path->string path1) }
      (file (path-add-extension path1 ".gz" ".")))
    )

  (printf "Creating backup: ~a\n\n" path) 
  ;{ touch (path->string path) } ; tar -rf will create a tar file if it doesn't exist yet

  (let* ([files-to-encrypt (stream-filter bak-file-encrypt? files)]
         [archive-encrypted 
           ; 1. archive files to be encrypted in a single tar.gz
           (let ([archive-to-be-encrypted 
                   (archive-files (string->path #{mktemp -u})
                                  files-to-encrypt)])
             (if archive-to-be-encrypted
               ; 2. encrypt the archive from 1.
               ; res : file?
               (let ([res (encrypt-file archive-to-be-encrypted)])
                 (if res
                   (begin
                     ; remove the original archive
                     ;#{rm -f (file-path archive-to-be-encrypted)}
                     ; mark the encrypted archive as a temporary file
                     (struct-copy bak-file (bak-file-from-file res) [temp? #t]))
                   #f))
               #f)
             )]
         [files-rest (stream-filter (negate bak-file-encrypt?) files)])
    (when archive-encrypted
      (debug "encrypted archive: ~a" (file-path archive-encrypted)))
    ; 3. include the encrypted archive from 2. into the final backup
    (archive-files path (if archive-encrypted
                          (stream-cons archive-encrypted files-rest)
                          files-rest))
    )

  (printf "\nBackup ready -> ~a\n" (string-append (path->string path) ".gz"))
  )

