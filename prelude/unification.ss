#!r6rs

(library (chrKanren prelude unification)
  (export ==)
  (import (rnrs)
          (chrKanren syntax)
          (chrKanren utils)
          (chrKanren vars)
          (chrKanren goals)
          (chrKanren rule))

  (define-constraint (== left right)
    (error '== "Should not reify ==" left right))

  (define-rules
    (forall (x) (== x x))
    (forall (x y)
      (== x y)
      (ground atom? x)
      (ground atom? y)
      (ground (negate equal?) x y)
      <=>
      fail)
    (forall (a b c d)
      (== (cons a b) (cons c d))
      <=>
      (== a c)
      (== b d))
    (forall (a b c)
      (== c (cons a b))
      (ground (negate pair?) c)
      <=>
      fail)
    (forall (a b c)
      (== (cons a b) c)
      (ground (negate pair?) c)
      <=>
      fail)
    (forall (x y)
      (== x y)
      (scheme var? x)
      <=>
      (=== x y))
    (forall (x y)
      (== x y)
      (scheme var? y)
      <=>
      (=== y x))))
