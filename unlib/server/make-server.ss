#lang scheme/base

(require "server-util.ss")

(begin (make-data-directories)
       (copy-configuration-files)
       (make-groups)
       (update-permissions))
