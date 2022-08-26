## Rackup
A backup utility made with Racket.

### Usage
Backup configurations are written as Racket programs that use `rackup` as a library dependency.

Run the script with `--help` to see the list of available options. You can test your backup script with `--simulate` or `--print-files`.

#### Example
```racket
#lang racket/base
(require rackup)

(make-backup "/tmp/test.bak"
             (files
               (in-dir "/etc"
                       "hosts"
                       "httpd")
               (in-dir "~/.config"
                       "mpv"
                       "pulse"
                       (in-dir "nvim"
                               "plugin"))
               (f "~/.gitconfig" #:encrypt? #t)))
```

The resulting backup will have the following structure inside:
```
/etc/
    hosts
    httpd/
~/.config
    mpv/
    pulse/
    nvim/
        plugin/
/tmp/tmp.<RANDOM>_encrypted.enc
```

All files that are marked for encryption are grouped together and stored in a single encrypted file, regardless of where they are defined in the backup script.

It's also possible to append the date to the resulting backup filename.

```racket
(make-backup "/tmp/test.bak"
             #:date-format "%d-%m-%Y"
             (files ...))
```

Or use the default format, which is `%Y-%m-%d_%Hh%Mm%Ss`:
```racket
(make-backup "/tmp/test.bak"
             #:append-date #t
             (files ...))
```


#### Combining with rash
[Rash](https://rash-lang.org/) allows you to mix Bash with Racket, which makes it convenient to define a backup script that saves an output of a bash command.

The following backup script saves a list of all installed packages with pacman and pip.
```racket
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
```

The resulting backup will have the following structure inside:
```
/etc/
    hosts
    httpd/
~/
    .bashrc
/tmp/tmp.<RANDOM>_stdout/
    pacman.list
    pip.list
```

All you need to know to make use of this is that `#{ <cmd> }` runs the given `<cmd>` and returns its output.
Also, notice that `#lang rash` must be used.
