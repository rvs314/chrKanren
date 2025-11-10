#!r6rs

(library (chrKanren prelude types)
  (export symbolo numbero)
  (import (rnrs)
          (chrKanren rule)
          (chrKanren goals)
          (chrKanren utils)
          (chrKanren syntax))

  ;; (define-constraint (has-typeo obj typ)
  ;;   (list typ obj))
  ;; (define-constriant (not-typeo obj typ)
  ;;   (list (string-append) typ))

  ;; (define (symbolo x) (has-typeo x 'symbolo))
  ;; (define (numbero x) (has-typeo x 'numbero))

  (define-constraint (symbolo obj) `(symbolo ,obj))
  (define-constraint (numbero obj) `(numbero ,obj))

  (define-rules
    (forall (x) (symbolo x) (ground symbol? x)          <=> succeed)
    (forall (x) (symbolo x) (ground (negate symbol?) x) <=> fail)
    (forall (x) (numbero x) (ground number? x)          <=> succeed)
    (forall (x) (numbero x) (ground (negate number?) x) <=> fail)
    (forall (x) (symbolo x) (numbero x)                 <=> fail)))
