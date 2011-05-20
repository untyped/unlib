#!/usr/bin/env mzscheme -q
#lang scheme

(require scheme/runtime-path
         scheme/system)

; Configuration ----------------------------------

; string
(define plt-version "4.2.2")

; path
(define-runtime-path planet-path "planet")

; -> string
(define (path-environment)
  (string-join (cons (format "/usr/local/plt-~a/bin" plt-version)
                     (filter (lambda (dir)
                               (not (regexp-match #rx"^/usr/local/plt[-0-9.]*/bin$" dir)))
                             (regexp-split #rx":" (getenv "PATH"))))
               ":"))

; Tasks ------------------------------------------

(define (env)
  (putenv "PLTVERSION" plt-version)
  (putenv "PLTPLANETDIR" (path->string planet-path))
  (putenv "PATH" (path-environment)))

(define (autoplanet)
  (env)
  (system "mzscheme autoplanet.ss"))

(define (envvars)
  (autoplanet)
  (let ([path (make-temporary-file "mzscheme-envvars-~a.sh")])
    (with-output-to-file path
      (lambda ()
        (printf #<<ENDSCRIPT
export PLTVERSION=~a
export PLTPLANETDIR="~a"
export PATH="~a"

ENDSCRIPT
                plt-version
                (path->string planet-path)
                (path-environment)))
      #:exists 'replace)
    (display (path->string path))))

(define (compile)
  (autoplanet)
  (system "mzc -v run-tests.ss"))

(define (test-compile)
  (autoplanet)
  (system "mzc -v run-tests.ss"))

(define (test)
  (test-compile)
  (system "mzscheme run-tests.ss"))

(define (docs)
  (autoplanet)
  (system "scribble ++xref-in setup/xref load-collections-xref --htmls scribblings/unlib.scrbl"))

(match (vector-ref (current-command-line-arguments) 0)
  ["envvars"         (envvars)]
  ["compile"         (compile)]
  ["test-compile"    (test-compile)]
  ["test"            (test)]
  ["docs"            (docs)])
