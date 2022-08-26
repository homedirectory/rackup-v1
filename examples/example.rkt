#!/usr/bin/env racket
#lang racket/base

(require rackup)

(make-backup "/tmp/test.bak"
             (files
               (in-dir "/etc"
                       "sudoers"
                       "httpd")
               (in-dir "~/.config"
                       "mpv"
                       "pulse"
                       (in-dir "nvim"
                               "plugin"))
               (f "~/.gitconfig" #:encrypt? #t)))
