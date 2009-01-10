#lang scheme/base

(require scheme/dict
         (file "base.ss")
         (file "generator.ss"))

; Provide statements --------------------------- 

(provide gen->
         (rename-out [generator-end  g:end]
                     [generator-end? g:end?]))

(provide/contract
 [rename generator-map               g:map               (->* (procedure?) () #:rest (listof procedure?) procedure?)]
 [rename generator-fold-map          g:fold-map          (->* (procedure? any/c) () #:rest (listof procedure?) procedure?)]
 [rename generator-filter            g:filter            (-> procedure? procedure? procedure?)]
 [rename generator-filter-map        g:filter-map        (-> procedure? procedure? procedure?)]
 [rename generator-remove-duplicates g:remove-duplicates (->* (procedure?) (procedure?) procedure?)]
 [rename generator-debug             g:debug             (-> string? procedure? procedure?)]
 [rename generator-for-each          g:for-each          (->* (procedure?) () #:rest (listof procedure?) any)]
 [rename generator-fold              g:fold              (->* (procedure? any/c) () #:rest (listof procedure?) any)]
 [rename generator->list             g:collect           (-> procedure? (or/c pair? null?))]
 [rename generator->hash             g:collect/hash      (->* (procedure? procedure?) 
                                                              (procedure? (and/c hash? dict-mutable?))
                                                              (and/c hash? dict-mutable?))]
 [rename list->generator             g:list              (-> list? procedure?)]
 [rename range->generator            g:range             (->* (integer?) ((or/c integer? false/c) integer?) procedure?)] 
 [rename generator-project           g:project           (->* ((listof boolean?) procedure?) (procedure?) procedure?)])
