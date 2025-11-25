#!r6rs

(library (chrKanren prelude types)
  (export symbolo numbero stringo typeo)
  (import (rnrs)
          (chrKanren rule)
          (chrKanren goals)
          (chrKanren utils)
          (chrKanren syntax))

  (define-constraint (typeo obj name _pred) (list name obj))

  (define-constraint (symbolo _obj) (error 'symbolo "Should not be reified"))
  (define-constraint (numbero _obj) (error 'numbero "Should not be reified"))
  (define-constraint (stringo _obj) (error 'stringo "Should not be reified"))

  (define-rules
    (forall (obj) (symbolo obj) <=> (typeo obj 'sym symbol?))
    (forall (obj) (numbero obj) <=> (typeo obj 'num number?))
    (forall (obj) (stringo obj) <=> (typeo obj 'str string?))
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
