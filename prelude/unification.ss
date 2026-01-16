#!r6rs

(library (chrKanren prelude unification)
  (export == occurs-checko)
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
      (scheme var? x)
      (scheme var? y)
      <=>
      (=== x y))
    (forall (x y)
      (== x y)
      (scheme var? y)
      (scheme (negate var?) x)
      <=>
      (== y x))
    (forall (x y)
      (== x y)
      (scheme (negate var?) y)
      (scheme var? x)
      <=>
      (occurs-checko x (list y) y))
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
      fail))

  ;; (occurs-checko X (Y ...) Z)
  ;; Assign the var X to Z if X does not literally occur
  ;; in any of Y ..., else fail

  (define-constraint (occurs-checko sub none-of-these kont)
    (error 'occurs-checko "Should not reify occurs-checko" sub none-of-these kont))

  (define-rules
    (forall (x k)
      (occurs-checko x '() k)
      <=>
      (=== x k))
    (forall (x z rs k)
      (occurs-checko x (cons (cons x z) rs) k)
      <=>
      fail)
    (forall (x z rs k)
      (occurs-checko x (cons (cons z x) rs) k)
      <=>
      fail)
    (forall (x y rs z k)
      (occurs-checko x (cons (cons y z) rs) k)
      (scheme (negate eq?) x y)
      (scheme (negate eq?) x z)
      <=>
      (occurs-checko x (cons* y z rs) k))
    (forall (x y rs k)
      (occurs-checko x (cons y rs) k)
      (scheme var? y)
      (scheme (negate eq?) x y)
      <=>
      (occurs-checko x rs k))
    (forall (x y rs k)
      (occurs-checko x (cons y rs) k)
      (ground atom? y)
      <=>
      (occurs-checko x rs k))))
