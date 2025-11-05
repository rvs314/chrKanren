#!r6rs

(library (chrKanren syntax)
  (export run run* conde define-relation)
  (import (rnrs)
          (chrKanren goals)
          (chrKanren streams)
          (chrKanren state)
          (chrKanren interp)
          (chrKanren vars)
          (chrKanren relation)
          (chrKanren utils)
          (chrKanren reifier)
          (srfi :39 parameters))

  (define-syntax-rule (run amt (var ...) goal ...)
    (fresh (var ...)
      (let* ([vs (list var ...)]
             [rs (take amt (start empty-state (conj goal ...)))])
        (map (lambda (r) (reify (reify-query vs r))) rs))))

  (define-syntax-rule (run* (var ...) goal ...)
    (run +inf.0 (var ...) goal ...))

  (define-syntax-rule (conde [conjunct ...] ...)
    (disj (conj conjunct ...) ...))

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
