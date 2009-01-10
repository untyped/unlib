#!/usr/bin/env mzscheme -qr
; This is a rough and ready script for compiling a bunch of JavaScript fragments
; into a single file. You write a "spec file" like this:
;
;     ----- SNIP -----
;     ("des1.js" ("src1a.js" "src1b.js" "src1c.js"))
;     ("des2.js" ("src2a.js" "src2b.js" "src2c.js"))
;     ("des3.js" ("src3a.js" "src3b.js" "src3c.js"))
;     ----- SNIP -----
;
; and run the script like:
;
;    jsmake.ss <spec-file>
;
; an optional "-R" command line parameter overwrites any existing destination files.

(require (lib "cmdline.ss")
         (lib "plt-match.ss")
         (file "base.ss")
         (file "file.ss"))

; Global variables -------------------------------

(define replace? #f)

; Procedures -------------------------------------

;; read-spec-file : (U path string) -> spec
;;
;; where spec : (list-of (list string (list-of string)))
;;
;; The spec structure is interpreted as:
;;
;;     (list-of (list des-filename (list-of src-filename)))
(define (read-spec path)
  (let ([in (open-input-file path)])
    (let loop ([data (read in)])
      (if (eof-object? data)
          null
          (cons data (loop (read in)))))))

(define (process-spec spec)
  (for-each (match-lambda
              [(list (? string? des) (? list? src))
               (printf "Concatenating files: ~a -> ~a...~n" src des)
               (for-each (lambda (src)
                           (unless (file-exists? src)
                             (raise-exn exn:unlib 
                               (format "File does not exist: ~a." src))))
                         src)
               (when (file-exists? des)
                 (if replace?
                     (delete-file des)
                     (raise-exn exn:fail:unlib 
                       (format "File exists: ~a (use the -R option to auto-replace)." des))))
               (concatenate-files des src)]
              [other
               (raise-exn exn:unlib 
                 (format "Badly formatted spec: ~a~n" other))])
            spec))

; Main script body -------------------------------

(command-line "jsmake" (current-command-line-arguments)
  (once-each
    [("-R" "--replace")
     "Replace existing files."
     (set! replace? #t)])
  (args (filename)
        (with-handlers ([exn:unlib?
                         (lambda (exn)
                           (printf "~a~n" (exn-message exn))
                           (printf "Bailing.~n")
                           (exit 1))])
          (process-spec (read-spec filename)))
        (printf "Done.~n")))

