#!r6rs

(import (rnrs)
        (chrKanren utils)
        (chrKanren test)
        (chrKanren check)
        (chrKanren vars)
        (chrKanren varmap)
        (chrKanren unify)
        (srfi :39 parameters))


(define-test smoke-tests
  (check (equal?
          empty-varmap
          (unify 1 1 empty-varmap)))
  (check (not (unify 1 2 empty-varmap))))

(define-test var-bindings
  (fresh (p q)
    (let* ([s0 empty-varmap]
           [s  (unify p q s0)])
      (check (eq? p (varmap-lookup p s0)))
      (check (eq? q (varmap-lookup q s0)))
      (check (eq? q (varmap-lookup p s)))
      (check (eq? q (varmap-lookup q s))))))

(define-test triangular-substitution
  (fresh (p q r)
    (let* ([s (unify (list p q) (list q r) empty-varmap)])
      (check (eq? q (varmap-lookup p s)))
      (check (eq? r (varmap-lookup q s)))
      (check (eq? r (varmap-lookup r s))))))

(define-test dual-pair
  (fresh (p q r)
    (let* ([s (unify `(,p . 1) `(2 . ,q) empty-varmap)])
      (check (equal? 2 (varmap-lookup p s)))
      (check (equal? 1 (varmap-lookup q s))))))
