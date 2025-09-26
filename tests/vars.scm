#!r6rs

(import (rnrs) (chrKanren test) (chrKanren vars) (srfi :39 parameters))

(define-test var-identity
  (define v1 (make-var 'v1))
  (define v2 (make-var 'v2))
  (check (eq? v1 v1))
  (check (not (eq? v1 v2)))
  (check (eq? (var-name v1) 'v1)))

(define-test test-fresh
  (fresh (p q)
    (check (eq? p p))
    (check (not (eq? p q)))
    (check (eq? (var-name p) 'p))))

(define-test test-var-counter
  (*var-counter* 0)
  (check (eqv? (var-idx (make-var 'the-var-counter)) 0))
  (check (eqv? (*var-counter*) 1))

  (parameterize ([*var-counter* 0])
    (check (eqv? (var-idx (make-var 'foo)) 0))
    (check (eqv? (var-idx (make-var 'bar)) 1))
    (check (eqv? (var-idx (make-var 'baz)) 2)))

  (check (eqv? (*var-counter*) 1)))
