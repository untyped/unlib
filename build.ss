#!/usr/bin/env mzscheme -q
#lang scheme

(require scheme/runtime-path
         scheme/system)

; Configuration ----------------------------------

; string
(define plt-version "4.2.1.5")

; path
(define-runtime-path planet-path "planet")

; Tasks ------------------------------------------

(define (env)
  (putenv "PLTVERSION" plt-version)
  (putenv "PLTPLANETDIR" (path->string planet-path)))

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

ENDSCRIPT
                plt-version
                (path->string planet-path)))
      #:exists 'replace)
    (display (path->string path))))

(define (compile)
  (autoplanet)
  (system "mzc -v main.ss"))

(define (test-compile)
  (autoplanet)
  (system "mzc -v run-tests.ss"))

(define (test)
  (test-compile)
  (system "mzscheme run-tests.ss"))

(match (vector-ref (current-command-line-arguments) 0)
  ["envvars"         (envvars)]
  ["compile"         (compile)]
  ["test-compile"    (test-compile)]
  ["test"            (test)])
