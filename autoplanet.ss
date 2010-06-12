#lang scheme

(require scheme/runtime-path
         (planet untyped/autoplanet:1))

(define-runtime-path dev-path
  "planetdev")

(remove-hard-links)

; No packages to install ...
; (install-local "owner" "package.plt" 1 0 (build-path dev-path "package")) ; ...
