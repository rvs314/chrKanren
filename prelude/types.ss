#!r6rs

(library (chrKanren prelude types)
  (export symbolo numbero)
  (import (rnrs)
          (chrKanren rule)
          (chrKanren goals)
          (chrKanren utils)
          (chrKanren syntax))

  (define-constraint (symbolo obj) `(symbolo ,obj))
  (define-constraint (numbero obj) `(num ,obj))

  (define-rules
    (forall (x) (symbolo x) (ground symbol? x)          <=> succeed)
    (forall (x) (symbolo x) (ground (negate symbol?) x) <=> fail)
    (forall (x) (numbero x) (ground number? x)          <=> succeed)
    (forall (x) (numbero x) (ground (negate number?) x) <=> fail)
    (forall (x) (numbero x) (numbero x)                 <=> (numbero x))
    (forall (x) (symbolo x) (symbolo x)                 <=> (symbolo x))
    (forall (x) (symbolo x) (numbero x)                 <=> fail)))
