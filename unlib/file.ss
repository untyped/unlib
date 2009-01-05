(module file mzscheme

  (require (lib "file.ss")
           (lib "plt-match.ss")
           (lib "port.ss")
           (lib "string.ss" "srfi" "13")
           (planet "read.ss" ("ryanc" "scripting.plt" 1)))

  (provide (all-defined))
  
  ;; make-directory-tree : (tree-of string) -> void
  ;;
  ;; Makes a tree of directories from a nested list specification:
  ;;
  ;;     <FOLDERS-SPEC> = (list <FOLDER-SPEC> ...)
  ;;     <FOLDER-SPEC>  = string <FOLDERS-SPEC>
  ;; 
  ;; For example:
  ;;     
  ;;     '("top1" 
  ;;        ("folder1" 
  ;;         ("subfolder1a" "subfolder1b")
  ;;         "folder2"
  ;;         ("subfolder2a" "subfolder2b"))
  ;;       "top2"
  ;;        ("folder1" 
  ;;         ("subfolder1a" "subfolder1b")
  ;;         "folder2"
  ;;         ("subfolder2a" "subfolder2b")))
  (define (make-directory-tree tree)
    (define (tree-fold seed tree)
      (define (list->path head rest)
        (apply build-path (reverse (cons head rest))))
      (match tree
        [(? string? here)
         (make-directory* (list->path here seed))]
        [(list) (void)]
        [`(,(? string? head) (,children ...) . ,rest)
         (make-directory* (list->path head seed))
         (tree-fold (cons head seed) children)
         (tree-fold seed rest)]
        [`(,(? string? here) . ,rest)
         (make-directory* (list->path here seed))
         (tree-fold seed rest)]))
    (tree-fold null tree))
  
  ;; make-non-conflicting-path : path string -> path
  ;;
  ;; The path version of make-non-conflicting-filename, explained
  ;; below:
  (define (make-non-conflicting-path path filename)
    (build-path path (make-non-conflicting-filename path filename)))
  
  ;; make-non-conflicting-filename : path string -> string
  ;;
  ;; Searches the specified path for any files whose name might conflict
  ;; with the suggested filename. If conflicting files are found, a
  ;; non-conflicting variant of filename is returned.
  ;; 
  ;; If no conflicting files are found, the filename is returned untouched.
  ;;
  ;; Non-conflicting names are generated in a nice, friendly, Windows-esque
  ;; kind of way, where a digit is appended to the end of the stem of the
  ;; file. There are a few subtleties to this: examples follow:
  ;;
  ;;   my-file.txt becomes:
  ;;
  ;;     my-file1.txt if my-file.txt exists
  ;;     my-file2.txt if my-file.txt and my-file1.txt exist
  ;;
  ;;   test-file becomes:
  ;;
  ;;     test-file1 if test-file exists
  ;;
  ;;   my-file5.txt becomes:
  ;;
  ;;     my-file6.txt if my-file5.txt exists
  ;;
  ;;   my-file1.blah.txt becomes:
  ;;
  ;;     my-file1.blah1.txt if my-file1.blah.txt exists
  (define (make-non-conflicting-filename path filename)
    ;; stem->stem-and-index : string -> (values string integer)
    ;;
    ;; Strips trailing digits off a string and returns them as a number.
    ;;
    ;; For example:
    ;;     "abc123" => (values "abc" 123)
    ;;     "abc" =? (values "abc" 1)
    (define (stem->stem-and-index stem)
      (let loop ([stem stem] [index-string ""])
        (if (char-numeric? (string-ref stem (sub1 (string-length stem))))
            (loop (string-drop-right stem 1) 
                  (string-append index-string (string-take-right stem 1)))
            (values stem
                    (if (= (string-length index-string) 0)
                        1
                        (string->number index-string))))))
    (if (file-exists? (build-path path filename))
        ; Split the filename into a stem and an extension
        (let* ([pos (string-index-right filename #\.)]
               [stem (if pos (string-take filename pos) filename)]
               [extension (if pos (string-drop filename pos) "")])
          ; Find a non-conflicting filename and return it
          (let-values ([(stem index) 
                        (stem->stem-and-index stem)])
            (let loop ([index index])
              (let ([filename 
                     (string-append 
                      stem
                      (number->string index) 
                      extension)])
                (if (file-exists? (build-path path filename))
                    (loop (add1 index))
                    filename)))))
        filename))
  
  ;; read-file->string : (U path string) -> string
  (define (read-file->string path)
    (let ([in  (open-input-file path)]
          [out (open-output-string)])
      (let loop ()
        (let ([buf (read-string 1024 in)])
          (unless (eof-object? buf)
            (display buf out)
            (loop))))
      (close-input-port in)
      (get-output-string out)))

  ;; concactenate-files : (U string path) (list-of (U string path)) -> void
  (define (concatenate-files des src)
    (with-output-to-file des
      (lambda ()
        (copy-port (apply input-port-append
                          #t
                          (map open-input-file src))
                   (current-output-port)))))
  
  )
