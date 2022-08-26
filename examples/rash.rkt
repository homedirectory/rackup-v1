#!/usr/bin/env racket
#lang rash

(require rackup)

(make-backup "/tmp/test-rash.bak"
             #:date-format "%Y-%m-%d"
             (files
               (in-dir "/etc"
                       "hosts"
                       "httpd")
               "~/.bashrc"
               (m "pacman.list" #{ pacman -Q })
               (m "pip.list" #{ pip3 list })))
