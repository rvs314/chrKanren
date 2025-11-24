#!r6rs

(library (chrKanren rule)
  (export forall parse-rule define-rules
          *constraint-handling-rules* define-rules
          forall <=>
          rule rule? rule-free-variables rule-prereqs rule-consequences
          ground)
  (import (rnrs)
          (srfi :39 parameters)
          (chrKanren check)
          (chrKanren utils)
          (chrKanren vars)
          (chrKanren goals))

  (define-record-type rule
    (fields free-variables prereqs consequences))

  (define *constraint-handling-rules* (make-parameter '()))

  (define-syntax forall (syntax-rules ()))
  (define-syntax <=> (syntax-rules ()))

  (define-syntax parse-rule
    (syntax-rules (forall <=>)
      [(parse-rule (vs ...) (gl ...) ())
       (parse-rule (vs ...) (gl ...) (<=> succeed))]
      [(parse-rule (vs ...) (gl ...) (<=> res ...))
       (fresh (vs ...)
         (make-rule
          (list vs ...)
          (list gl ...)
          (lambda (vs ...) (conj res ...))))]
      [(parse-rule (vs ...) (gl ...) (g₀ rest ...))
       (parse-rule (vs ...) (gl ... g₀) (rest ...))]
      [(parse-rule (forall (vs ...) rest ...))
       (parse-rule (vs ...) () (rest ...))]))

  (define-syntax-rule (define-rules rule ...)
    (define frombnicus
      (*constraint-handling-rules*
       (cons* (parse-rule rule) ... (*constraint-handling-rules*)))))

  (define (ground pred . os)
    (define (any-vars? . os) (exists var? os))
    (apply scheme (conjoin (negate any-vars?) pred) os)))
