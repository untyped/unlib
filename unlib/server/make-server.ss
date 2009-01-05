#lang scheme/base

(require (file "server-util.ss"))

(begin (make-data-directories)
       (copy-configuration-files)
       (make-groups)
       (update-permissions))
