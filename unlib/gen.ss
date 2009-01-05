(module gen mzscheme

  (require (lib "contract.ss"))
  
  (require (file "generator.ss"))

  ; Provide statements --------------------------- 
  
  (provide gen->
           list->generator)
  
  (provide
   [rename generator-end               g:end]
   [rename generator-end?              g:end?])

  (provide/contract
   [rename generator-map               g:map               (->* (procedure?) (listof procedure?) (procedure?))]
   [rename generator-fold-map          g:fold-map          (->* (procedure? any/c) (listof procedure?) (procedure?))]
   [rename generator-filter            g:filter            (-> procedure? procedure? procedure?)]
   [rename generator-filter-map        g:filter-map        (-> procedure? procedure? procedure?)]
   [rename generator-remove-duplicates g:remove-duplicates (opt-> (procedure?) (procedure?) procedure?)]
   [rename generator-debug             g:debug             (-> string? procedure? procedure?)]
   [rename generator-for-each          g:for-each          (->* (procedure?) (listof procedure?) any)]
   [rename generator-fold              g:fold              (->* (procedure? any/c) (listof procedure?) any)]
   [rename generator->list             g:collect           (-> procedure? (or/c pair? null?))]
   [rename generator-project           g:project           (opt-> ((listof boolean?) procedure?) (procedure?) procedure?)])
   
  )