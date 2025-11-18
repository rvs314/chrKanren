#!r6rs

(library (chrKanren tests fmk shim)
  (export test defrel)
  (import (rnrs)
          (chrKanren utils)
          (chrKanren base)
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
    (define-relation arg ...)))
