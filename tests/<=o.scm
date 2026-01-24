#!r6rs

(import (rnrs) (chrKanren base) (chrKanren test))

(define-constraint (<=o lesser greater))

(define-rules
  (forall (x)     (<=o x x)           <=> succeed)
  (forall (x y)   (<=o x y) (<=o y x) <=> (== x y))
  (forall (x y z) (<=o x y) (<=o y z) <=> (<=o x y) (<=o y z) (<=o x z)))

;; According to Thom Frühwirth, this is supposed to terminate
;; if constraint conjunction is idempotent, but it isn't.
;; Why does he use this example everywhere?
(define-test thoms-test
  (test-count 0)
  (check (equal?
          (run* (a b c) (<=o a b) (<=o c a) (<=o b c))
          '((_.0 _.0 _.0)))))
