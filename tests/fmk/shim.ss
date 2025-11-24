#!r6rs

(library (chrKanren tests fmk shim)
  (export test
          defrel
          (rename (run-shim run)
                  (run*-shim run*)))
  (import (rnrs)
          (chrKanren utils)
          (chrKanren base)
          (chrKanren compare)
          (chrKanren check)
          (chrKanren test))

  (define-syntax test
    (lambda (stx)
      (syntax-case stx ()
        [(test nm operation result)
         (with-syntax ([new-name
                        (cond [(string? (syntax->datum #'nm))
                               (datum->syntax #'test
                                              (string->symbol
                                               (syntax->datum #'nm)))]
                              [(identifier? #'nm) #'nm]
                              [else #'check-test])])
           #'(define-test new-name
               (check (equal? operation result) nm)))])))

  (define-syntax-rule (defrel arg ...)
    (define-relation arg ...))

  (define (constraint-sort cs)
    (define (disc obj) (eqv<=? (lambda (o) (eq? o obj))))
    (define constraint<=?
      (prioritize (disc '=/=)
                  (disc 'num)
                  (disc 'sym)
                  (disc 'str)
                  (disc 'absento)))
    (sort (make-pair<=? constraint<=? lex<=?) cs))

  (define (shim res)
    (define (shim-constraint con)
      (define kind (car con))
      (define instances (cdr con))
      (cons kind
            (lex-sort
             (case kind
               ((sym str num) (map car instances))
               ((=/=) (map (compose lex-sort (lambda (x) (map lex-sort x))) instances))
               (else instances)))))

    (define (shim-result res)
      (define term (single-out (car res)))
      (define constraints
        (constraint-sort
         (map shim-constraint (group-by car+cdr (cdr res)))))
      (single-out (cons term constraints)))

    (map shim-result res))

  (define-syntax-rule (run-shim arg ...)
    (shim (run-finite arg ...)))

  (define-syntax-rule (run*-shim arg ...)
    (shim (run*-finite arg ...))))
