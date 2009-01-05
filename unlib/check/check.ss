#lang scheme/base

(require (for-syntax scheme/base
                     (file "../syntax.ss"))
         (only-in srfi/1/list append-map filter filter-map)
         (only-in (planet schematics/schemeunit:2/text-ui) display-exn)
         (file "../base.ss")
         (file "../list.ss"))

; Result structures ----------------------------

(define-struct check-result (message annotations) #:transparent)
(define-struct (check-problem check-result) () #:transparent)
(define-struct (check-success check-result) () #:transparent)
(define-struct (check-failure check-problem) () #:transparent)
(define-struct (check-warning check-problem) () #:transparent)
(define-struct (check-error   check-problem) (exn) #:transparent)

; check-result -> (U exn #f)
(define (check-result-exn result)
  (if (check-error? result)
      (check-error-exn result)
      #f))

; Result constructors --------------------------

; -> (list check-success)
(define (pass)
  (list (make-check-success "Okay" null)))

; string -> (list check-failure)
(define (fail message)
  (list (make-check-failure message null)))

; string -> (list check-warning)
(define (warn message)
  (list (make-check-warning message null)))

; (-> (listof check-result)) -> (listof check-result)
(define (check-with-handlers thunk)
  (with-handlers ([exn? (lambda (exn)
                          (display-exn exn)
                          (list (make-check-error (if (exn:fail? exn)
                                                      "Exception raised"
                                                      (exn-message exn))
                                                  null exn)))])
    (thunk)))

; Result list combinators ----------------------

; (listof check-result) ... -> (listof check-result)
(define check-all append)

; (alistof symbol any) (listof check-result) -> (listof check-result)
(define (check-with-annotations annotations results)
  (map (cut annotate-check-result <> annotations)
       results))

; (-> (listof check-result)) ... -> (listof check-results)
(define check-until-problems
  (match-lambda*
    [(list) null]
    [(list-rest head tail)
     (let* ([results  (head)]
            [problems (check-results->problems results)])
       (if (null? problems)
           (apply check-until-problems tail)
           results))]))

; (listof check-result) -> boolean
(define (check-problems? results)
  (ormap check-problem? results))

; (listof check-result) -> (listof (U check-warning check-failure check-error))
(define (check-results->problems results)
  (define-values (warnings failures errors)
    (check-results->warnings+failures+errors results))
  (append errors failures warnings))

;  (listof check-result)
; ->
;  (listof check-warning)
;  (listof check-failure)
;  (listof check-error)
(define (check-results->warnings+failures+errors results)
  (let loop ([results results]
             [warnings null]
             [failures null]
             [errors null])
    (match results
      [(list)
       (values (reverse warnings)
               (reverse failures)
               (reverse errors))]
      [(list-rest (? check-success? result) other)
       (loop other 
             warnings
             failures
             errors)]
      [(list-rest (? check-warning? result) other)
       (loop other 
             (cons result warnings)
             failures
             errors)]
      [(list-rest (? check-failure? result) other)
       (loop other 
             warnings
             (cons result failures)
             errors)]
      [(list-rest (? check-error? result) other)
       (loop other 
             warnings
             failures
             (cons result errors))])))

; Annotations ----------------------------------

; check-result symbol -> boolean
(define (check-result-annotation? result key)
  (if (assoc key (check-result-annotations result))
      #t
      #f))

; check-result symbol -> any
(define (check-result-annotation result key)
  (assoc-value key (check-result-annotations result)))

; check-result symbol any -> any
(define (check-result-annotation/default result key default)
  (assoc-value/default key (check-result-annotations result) default))

; check-result (alistof symbol any) -> check-result
(define (annotate-check-result result annotations)
  (let ([message (check-result-message result)]
        [annotations (alist-merge (check-result-annotations result) annotations 'second)])
    (cond [(check-success? result) (make-check-success message annotations)]
          [(check-failure? result) (make-check-failure message annotations)]
          [(check-warning? result) (make-check-warning message annotations)]
          [(check-error? result)   (make-check-error   message annotations (check-error-exn result))])))

; Provide statmenets ---------------------------

(provide/contract
 [struct check-result                     ([message string?] [annotations (listof (cons/c symbol? any/c))])]
 [struct (check-success check-result)     ([message string?] [annotations (listof (cons/c symbol? any/c))])]
 [struct (check-problem check-success)    ([message string?] [annotations (listof (cons/c symbol? any/c))])]
 [struct (check-warning check-problem)    ([message string?] [annotations (listof (cons/c symbol? any/c))])]
 [struct (check-failure check-problem)    ([message string?] [annotations (listof (cons/c symbol? any/c))])]
 [struct (check-error   check-problem)    ([message string?] [annotations (listof (cons/c symbol? any/c))] [exn exn?])]
 
 [check-result-exn                        (-> check-result? (or/c exn? false/c))]
 
 [pass                                    (-> (list/c check-success?))]
 [fail                                    (-> string? (list/c check-failure?))]
 [warn                                    (-> string? (list/c check-warning?))]
 
 [check-all                               (->* () () #:rest (listof (listof check-result?)) (listof check-result?))]
 [check-with-handlers                     (-> (-> (listof check-result?)) (listof check-result?))]
 [check-with-annotations                  (-> (listof (cons/c symbol? any/c)) (listof check-result?) (listof check-result?))]
 [check-until-problems                    (->* () () #:rest (listof procedure?) (listof check-problem?))]
 
 [check-problems?                         (-> (listof check-result?) boolean?)]
 [check-results->problems                 (-> (listof check-result?) (listof check-problem?))]
 [check-results->warnings+failures+errors (-> (listof check-result?)
                                              (values (listof check-warning?)
                                                      (listof check-failure?)
                                                      (listof check-error?)))]
 
 
 [check-result-annotation?                (-> check-result? symbol? boolean?)]
 [check-result-annotation                 (-> check-result? symbol? any/c)]
 [check-result-annotation/default         (-> check-result? symbol? any/c any/c)]
 [annotate-check-result                   (-> check-result? (listof (cons/c symbol? any/c)) check-result?)])
