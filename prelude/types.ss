#!r6rs

(library (chrKanren prelude types)
  (export symbolo numbero stringo typeo)
  (import (rnrs)
          (chrKanren rule)
          (chrKanren goals)
          (chrKanren utils)
          (chrKanren syntax))

  (define-constraint (typeo obj name _pred) (list name obj))

  (define (symbolo obj) (typeo obj 'sym symbol?))
  (define (numbero obj) (typeo obj 'num number?))
  (define (stringo obj) (typeo obj 'str string?))

  (define-rules
    (forall (o n p)
      (typeo o n p)
      (ground (lambda (p o) (p o)) p o)
      <=>
      succeed)
    (forall (o n p)
      (typeo o n p)
      (ground (lambda (p o) (not (p o))) p o)
      <=>
      fail)
    (forall (o n p)
      (typeo o n p)
      (typeo o n p)
      <=>
      (typeo o n p))
    (forall (o n n^ p p^)
      (typeo o n p)
      (typeo o n^ p^)
      (ground (negate eq?) n n^)
      <=>
      fail)
    (forall (o n p vs)
      (reifying vs)
      (typeo o n p)
      (scheme (lambda (o vs)
                (not (find-subtree
                      (lambda (needle) (eq? o needle))
                      vs)))
              o
              vs)
      <=>
      (reifying vs))
    (forall (o n n^ p p^)
      (typeo o n p)
      (typeo o n^ p^)
      (ground (negate eq?) p p^)
      <=>
      fail)))
