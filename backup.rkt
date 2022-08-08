#!/usr/bin/env racket
#lang rash

(require racket/cmdline racket/list racket/stream racket/string racket/bool
         racket/promise)
(require linea/line-macro)
(require shell/pipeline)

(provide mk-backup f m in-dir)
;(provide (all-defined-out))

(define-syntax-rule (alias-proc alias-id proc-id)
                    (define-syntax-rule (alias-id args (... ...)) (proc-id args (... ...))))

; --- file structure ---
; for unix files/directories/symlinks
; path : path?
(struct file ([path #:mutable]))

; path : path?
(define (file/dir-exists? path)
  (not (false? (file-or-directory-type path #f))))
(define (dir? afile) (eq? 'directory (file-or-directory-type afile #f)))
(define (link? afile) (eq? 'link (file-or-directory-type afile #f)))
(define (can-read? afile)
  (or (mbf? afile) 
      (and (file/dir-exists? (file-path afile)) 
           (not (false? (memq 'read 
                              (file-or-directory-permissions (file-path afile))))))))

; --- bak-file structure ---
; for files that are backed up
(struct bak-file file (encrypt? temp?))
; bak-file constructor
(define (mk-bak-file path-str #:encrypt [encrypt? #f] #:symlink [symlink? #f])
  ;(printf "f ~a\n" path-str) 
  (let* ([path (expand-user-path path-str)])
    ;(if (false? type) 
    ;(begin (printf "warn: ~a doesn't exist\n" path) '())
    ;{echo $path-str}
    (bak-file path encrypt? #f))
  )

(alias-proc f mk-bak-file)

; --- mbf (mem-bak-file) structure ---
; for in-memory contents that need to be backed up
; tmp-dir-path : path?
(struct mbf bak-file (data-delayed [tmp-dir-path #:mutable #:auto])
        #:auto-value #f)
; mbf constructor
; data : promise?
(define (mk-mbf name data)
    (mbf (build-path "stdout" name) #f #t data))

(define (get-mbf-data obj)
  (force (mbf-data-delayed obj)))

(define (get-mbf-full-path obj)
  (build-path (mbf-tmp-dir-path obj) (file-path obj)))

(define (touch-mbf obj)
    (set-mbf-tmp-dir-path! obj (string->path #{ mktemp -d }))
    (let ([mbf-full-path-string (path->string (get-mbf-full-path obj))])
    {
      mkdir -p #{dirname "$mbf-full-path-string"}
      echo (get-mbf-data obj) &>! "$mbf-full-path-string"
    }
    obj
    ))

; macro for creating mbf with delayed data
(define-syntax-rule (m name data)
                    (mk-mbf name (delay data)))


; --- file processing ---
(define (encrypt afile)
  ;{
  ;    gpg --batch --yes -c -o $(mktemp) (file-path afile)
  ;}
  (displayln "WARN: encryption is not supported")
  afile
  )


; --- helper procedures ---
;(define (path-join . paths)
;  (string-join (map (lambda (p) (string-trim p "/" #:left? #f)) paths) "/"))

(define (verify-path path overwrite?)
  (when (file/dir-exists? path)
    (if overwrite? 
      { rm -rf (path->string path) }
      (raise-user-error (format "ERROR: file already exists: ~a" path))
      )))

(define (to-file x)
  (cond [(file? x) x]
        [(string? x) (f x)]
        [else (error (format "Unacceptable file candidate: ~e" x))]))

(define (to-file/list x)
  (cond [(list? x) (map to-file (flatten x))]
        [else (to-file x)]))

; args : (or/c string? file?)
(define (in-dir dir-path-str . args)
  (map (lambda (x)
         (set-file-path! x (build-path (expand-user-path dir-path-str) (file-path x)))
         x)
       ; flatten removes nulls
       (to-file/list args)))


; --- main backup logic ---
(define (simulate path files)

  (define (sim-process-file afile)
    (if (not (can-read? afile))
      (printf "WARN: can't read file: ~a\n" (file-path afile))
      (cond [(bak-file-encrypt? afile) (printf "~a - encrypt\n" (file-path afile))]
            [(mbf? afile) (printf "mbf: ~a\n" (build-path #{ mktemp -u } (file-path afile)))]
            [else (printf "~a\n" (file-path afile))])))

  {
      echo -e "Simulating backup: $path\n"
      (for-each sim-process-file files)
      echo -e "\nDone"
  }
  )

(define (archive path files)
  (define (pre-process-file afile)
    (cond [(bak-file-encrypt? afile) (encrypt afile)]
          [(mbf? afile) (touch-mbf afile)]
          [else afile])
    )
  (define (archive-file afile)
    (let ([afile-path (file-path afile)])
      ; order of cond predicates matters since we have a structure hierarchy
      (cond [(mbf? afile) 
             { tar -C (path->string (mbf-tmp-dir-path afile)) -rPh -f (path->string path) (path->string afile-path) }]
            [(bak-file? afile) 
             { tar -rPh -f (path->string path) (path->string afile-path) }]
            )
      afile))
  (define (post-process-file afile)
    (cond [(bak-file-temp? afile) 
           (cond [(mbf? afile) 
                  { rm -r (path->string (mbf-tmp-dir-path afile)) }]
                 [else 
                   { rm -r (path->string (file-path afile)) }])]
          )
    afile)


    (printf "Creating backup: ~a\n" path)
    {
      touch (path->string path)
    }
  (for-each (lambda (afile)
              (let* ([afile (pre-process-file afile)]
                     [afile-path (file-path afile)])
                (with-handlers 
                  ([exn:fail? (lambda (e) 
                                (printf "WARN: can't backup: ~a\n" afile-path))])
                  (archive-file afile))
                (post-process-file afile)))
            files)
  {
    gzip -f (path->string path)
    echo -e "\nBackup ready -> " (string-append (path->string path) ".gz")
  }
  )

(define-syntax mk-backup
  (syntax-rules ()
    [(mk-backup bak-path args ...) (make-backup bak-path (stream args ...))]))

; args : (streamof (or/c string? file? (listof args)))
(define (args-stream->files args)
  (flatten (stream->list (stream-map to-file/list args))))

; args : (streamof (or/c string? file? (listof args)))
(define (make-backup out-path-str args)
  (define simulate? (make-parameter #f))
  (define out-path (make-parameter (string->path out-path-str)))
  (define overwrite? (make-parameter #f))
  (command-line 
    #:once-each
    [("-s" "--simulate") "Don't create any files"
                         (simulate? #t)]
    [("-o" "--output") output "Output file (takes precedence over the defined one)"
                       (out-path (string->path output))]
    [("--overwrite") "Overwrite output file if it exists"
                       (overwrite? #t)])


  (verify-path (out-path) (overwrite?))
  (let ([files (args-stream->files args)])
    (cond [(simulate?) (simulate (out-path) files)]
          [else (archive (out-path) files)]))
  )


