#!r6rs

(import (rnrs)
        (chrKanren test)
        (chrKanren vars)
        (chrKanren base)
        (srfi :39 parameters)
        (chrKanren syntax))

(define-test var-identity
  (define v1 (make-var 'v1))
  (define v2 (make-var 'v2))
  (check (eq? v1 v1))
  (check (not (eq? v1 v2)))
  (check (eq? (var-name v1) 'v1)))

(define-test test-fresh
  (fresh (p q)
    (begin
      (check (eq? p p))
      (check (not (eq? p q)))
      (check (eq? (var-name p) 'p))
      succeed)))

(define-test test-var-counter
  (*var-counter* 0)
  (check (var? (make-var 'the-var-counter)))
  (check (eqv? (*var-counter*) 1))

  (parameterize ([*var-counter* 0])
    (let ([foo (make-var 'foo)]
          [bar (make-var 'bar)]
          [baz (make-var 'baz)])
      (check (var? foo))
      (check (var? bar))
      (check (var? baz))
      (check (not (eq? foo bar)))
      (check (not (eq? foo baz)))
      (check (not (eq? bar baz)))
      (check (eqv? (*var-counter*) 3))))

  (check (eqv? (*var-counter*) 1)))
