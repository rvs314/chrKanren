#!r6rs

(import (rnrs)
        (chrKanren test)
        (chrKanren utils)
        (chrKanren base)
        (chrKanren syntax)
        (chrKanren vars)
        (chrKanren state)
        (chrKanren reifier)
        (chrKanren streams)
        (chrKanren interp)
        (chrKanren unify)
        (chrKanren rule)
        (chrKanren varmap)
        (chrKanren prelude disunification)
        (chrKanren goals)
        (only (racket) define-values)
        (prefix (srfi :41 streams) s:)
        (only (srfi :1 lists) lset=))


(define-constraint (<=o lesser greater))

(define-rules
  (forall (x)
    (forget (<=o x x)))
  (forall (p q)
    (forget (<=o p q))
    (<=o p q))
  (forall (x y)
    (forget (<=o x y))
    (forget (<=o y x))
    <=>
    (=== x y))
  (forall (x y z)
    (<=o x y)
    (<=o y z)
    <=>
    (<=o x z)))
