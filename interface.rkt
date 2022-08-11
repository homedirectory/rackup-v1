#lang rash

(require racket/list racket/string racket/function racket/cmdline
         racket/promise)
(require "helpers.rkt" "file-struct.rkt" "backup.rkt" "macros.rkt"
         "logging.rkt")

(provide (all-defined-out))

; args : (listof string? file? (listof ...))
; returns a list of files with prefixed paths
(define (in-dir dir-path-str . args)
  (let ([path-prefix 
          (if (string-empty? dir-path-str) "" (expand-user-path dir-path-str))])
    (map 
      (lambda (x)
        ;(debug "in-dir: ~a" x)
        (cond [(bak-file? x) 
               (set-file-path! 
                 x 
                 (my-build-path path-prefix (file-path x)))
               x]
              [(string? x) (mk-bak-file (my-build-path path-prefix x))]
              [else (error (format "in-dir: Unacceptable file candidate: ~e" x))]
              ))
      (filter 
        (negate (lambda (x) (and (string? x) (string-empty? x))))
        (flatten args)))))

(define-syntax-rule
  (files args ...) 
  (in-dir "" args ...))

; macro alias for creating bak-file
(alias-proc f mk-bak-file)
; macro for creating mbf with delayed data
(define-syntax-rule (m name data)
                    (mk-mbf name (delay data)))


(define (verify-path path overwrite?)
  (when (file/dir-exists? path)
    (if overwrite? 
      { rm -rf (path->string path) }
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
  (command-line 
    #:once-each
    [("-s" "--simulate") "Don't create any files"
                         (opt-simulate? #t)]
    [("-o" "--output") --output "Output file (takes precedence over the defined one)"
                       (opt-out-path (string->path --output))]
    [("--overwrite") "Overwrite output file if it exists"
                     (opt-overwrite? #t)])

  (let* ([append-date? (or append-date?
                           (not (string-empty? date-format)))]
         [date-format (if (string-empty? date-format)
                        "%Y-%m-%d_%H:%M:%S"
                        date-format)]
         [out-path (if append-date?
                     (path-replace-extension 
                       (opt-out-path) 
                       (string-append "_" 
                                      #{date (string-append "+" date-format)} 
                                      (val-or (my-path-get-extension (opt-out-path)) "")))
                     (opt-out-path))])
    (verify-path out-path (opt-overwrite?))
    (cond [(opt-simulate?) (simulate out-path files)]
          [else (archive out-path files)]))
  )


