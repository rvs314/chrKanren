#!r6rs

(library (chrKanren syntax)
  (export run run* conde)
  (import (rnrs)
          (chrKanren goals)
          (chrKanren streams)
          (chrKanren state)
          (chrKanren interp)
          (chrKanren subst)
          (chrKanren vars)
          (chrKanren utils)
          (srfi :39 parameters))

  (define-syntax-rule (run amt (var ...) goal ...)
    (parameterize ([*var-counter* 0])
      (fresh (var ...)
        (let ([vs (list var ...)]
              [rs (take amt (start empty-state (conj goal ...)))])
          (map (lambda (r) (reify vs r)) rs)))))

  (define-syntax-rule (run* (var ...) goal ...)
    (run +inf.0 (var ...) goal ...))

  (define-syntax-rule (conde [conjunct ...] ...)
    (disj (conj conjunct ...) ...)))
