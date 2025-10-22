#!r6rs

(library (chrKanren syntax)
  (export run run* conde fresh define-relation)
  (import (rnrs)
          (chrKanren goals)
          (chrKanren streams)
          (chrKanren state)
          (chrKanren interp)
          (chrKanren vars)
          (chrKanren relation)
          (chrKanren utils)
          (srfi :39 parameters))

  (define-syntax-rule (run amt (var ...) goal ...)
    (parameterize ([*var-counter* 0])
      (fresh (var ...)
        (let ([vs (list var ...)]
              [rs (take amt (start empty-state (conj goal ...)))])
          (map (lambda (r) (reify-query vs r)) rs)))))

  (define-syntax-rule (run* (var ...) goal ...)
    (run +inf.0 (var ...) goal ...))

  (define-syntax-rule (conde [conjunct ...] ...)
    (disj (conj conjunct ...) ...))

  (define-syntax-rule (fresh (var-name ...)
                        body body* ...)
    (let ([var-name (make-var 'var-name)]
          ...)
      (conj body body* ...)))

  (define-syntax-rule (define-relation (name arg ...)
                        body body* ...)
    (define the-relation
      (make-relation
       'name
       (list 'arg ...)
       (lambda (arg ...)
         (conj body body* ...))))

    (define (name arg ...)
      (Zzz (make-call the-relation (list arg ...))))))
