#!r6rs

(import (rnrs) (chrKanren base) (chrKanren test))

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
    =>
    (=== x y))
  (forall (x y z)
    (<=o x y)
    (<=o y z)
    =>
    (<=o x z)))

(define-test thoms-test
  (check (equal?
          (run* (a b c) (<=o a b) (<=o c a) (<=o b c))
          '(((_.0 _.0 _.0))))))
