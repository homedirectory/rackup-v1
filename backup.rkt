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
  (stream-rec-for-each sim-process-file files)
  (printf "\nDone\n")
  )

(define (archive path files)
  (define (pre-process-file fil)
    (cond [(bak-file-encrypt? fil) (encrypt-file fil)]
          [(mbf? fil) (touch-mbf fil)]
          [else fil])
    )
  (define (archive-file fil)
    (let ([fpath (file-path fil)])
      ; order of cond predicates matters since we have a structure hierarchy
      (cond [(mbf? fil) 
             { tar -C (path->string (mbf-tmp-dir-path fil)) -rPh -f (path->string path) (path->string fpath) }]
            [(bak-file? fil) 
             { tar -rPh -f (path->string path) (path->string fpath) }]
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

  (printf "Creating backup: ~a\n\n" path) 
  { touch (path->string path) }

  (stream-rec-for-each 
    (lambda (fil)
      (let* ([fil (pre-process-file fil)]
             [fpath (file-path fil)])
        (with-handlers 
          ([exn:fail? (lambda (e) 
                        (warn "can't backup: ~a" fpath))])
          (archive-file fil))
        (info "archived: ~a" fpath)
        (post-process-file fil)))
    files)

  { gzip -f (path->string path) }
  (printf "\nBackup ready -> ~a\n" (string-append (path->string path) ".gz"))
  )

