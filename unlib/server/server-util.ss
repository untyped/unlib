#lang scheme/base

(require "../base.ss"
         "../file.ss"
         mzlib/process)

; Procedures -----------------------------------

; void -> void
;
; Create a hierarchy of directories under /
(define (make-data-directories)
  (parameterize
      ((current-directory "/"))
    (make-directory-tree
     '("data" ("www" "log" "conf" ("www" ("auth" "web-server")))))))

; void -> void
;
; Create the admin and www-data groups
(define (make-groups)
  (system "addgroup admin")
  (system "addgroup www-data"))

; void -> void
;
; Copy configuration files out of /etc/ into /data/conf, and symlink back into /etc/
(define (copy-configuration-files)
  (move-and-link "apache2" "/etc" "/data/conf/www")
  (move-and-link "apache2" "/var/log" "/data/log")
  (delete-link "/data/conf/www/apache2/sites-enabled/000-default"))

; void -> void
;
; Updates permissions so:
;   - admin group own and has rw permissions on everything in /data
(define (update-permissions)
  (system "chgrp -R admin /data")
  (system "chmod -R g+rw /data"))

; string string string -> void
;
; Moves item from src to des and creates a symlink from src/item to des/item.
;
; If src is already a symlink, does nothing.
(define (move-and-link item src des)
  (if (or (link-exists? (format "~a/~a" src item))
          (file-exists? (format "~a/~a" des item))
          (directory-exists? (format "~a/~a" des item)))
      (printf "move-and-link: Skipping ~a ~a ~a as one or more already exist\n" item src des)
      (begin
        (system (format "mv ~a/~a ~a" src item des))
        (system (format "ln -s ~a/~a ~a/~a" des item src item)))))

; path -> void
(define (delete-link path)
  (if (link-exists? path)
      (delete-file path)
      (printf "delete-link: Skipping ~a - either it does not exist or it isn't a link\n" path)))

; Provide statements --------------------------- 

(provide make-data-directories
         make-groups
         copy-configuration-files
         update-permissions)
