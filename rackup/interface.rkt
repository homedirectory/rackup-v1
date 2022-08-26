#lang racket/base

(require racket/list racket/string racket/function racket/cmdline
         racket/promise racket/system)
(require "helpers.rkt" "file-struct.rkt" "backup.rkt" "macros.rkt"
         "logging.rkt")

(provide (all-defined-out))

; args : (listof string? file? (listof ...))
; returns a list of files with prefixed paths
; if exclude? = #t, then return a bak-dir struct object that excludes args
;   and args = (listof string?)
(define (in-dir dir-path-str #:exclude? [exclude? #f] #:encrypt? [encrypt? #f] . args)
  ; pre-process args 
  (let ([args (filter 
                (negate (lambda (x) (and (string? x) (string-blank? x))))
                (flatten args))])
    (mk-bak-dir dir-path-str #:exclude? exclude? #:encrypt? encrypt? args)))

(define-syntax-rule
  (files args ...) 
  (bak-dir->list (in-dir "" args ...)))

; TODO globs
;(define (glob str))

; macro alias for creating bak-file
(alias-proc f mk-bak-file)
; macro for creating mbf with delayed data
(define-syntax-rule (m name data)
                    (mk-mbf name (delay data)))


(define (verify-path path overwrite?)
  (when (file/dir-exists? path)
    (if overwrite? 
      (system (format "rm -rf ~s" (path->string path)))
      (raise-user-error (format "ERROR: file already exists: ~a" path))
      )))

; files : (listof file?)
(define (make-backup out-path-str 
                     #:overwrite? [overwrite? #f]
                     #:append-date? [append-date? #f] 
                     #:date-format [date-format ""]
                     files)
  (define opt-simulate? (make-parameter #f))
  (define opt-out-path (make-parameter (string->path out-path-str)))
  (define opt-overwrite? (make-parameter overwrite?))
  ;(define opt-print-sizes? (make-parameter #f)) ; useful with simulation
  (define opt-print-files? (make-parameter #f)) ; useful for piping to other commands
  (command-line 
    #:once-each
    [("-s" "--simulate") "Don't create any files"
                         (opt-simulate? #t)]
    [("-o" "--output") --output "Output file (takes precedence over the defined one)"
                       (opt-out-path (string->path --output))]
    [("--overwrite") "Overwrite output file if it exists"
                     (opt-overwrite? #t)]
    ;[("--print-sizes") "Print size for each file (useful when simulating)"
    ;                (opt-print-sizes? #t)]
    [("--print-files") "Simply print a list of files to be backed up (useful for piping with other commands)"
                       (opt-print-files? #t)])

  (if (opt-print-files?)
    (for-each (lambda (x) (displayln (file-path x)))
              ; filter mbf's, since they are non-existent files
              (filter (negate mbf?) files))
    (let* ([append-date? (or append-date?
                             (not (string-blank? date-format)))]
           [date-format (if (string-blank? date-format)
                          "%Y-%m-%d_%Hh%Mm%Ss"
                          date-format)]
           [out-path (if append-date?
                       (path-replace-extension 
                         (opt-out-path) 
                         (string-append "_" 
                                        (system (format "date ~a" (fix-date-cmd-arg date-format)))
                                        (val-or (my-path-get-extension (opt-out-path)) "")))
                       (opt-out-path))])

      (if (opt-simulate?) 
        (simulate out-path files)
        (begin 
          (verify-path out-path (opt-overwrite?))
          (mk-backup out-path files)))))
  )


