#!r6rs

(import (rnrs)
        (chrKanren test)
        (chrKanren vars)
        (chrKanren base)
        (srfi :39 parameters)
        (chrKanren syntax))

(define-test smoke-tests
  (check (equal? (run* () (=== 1 1))
                 '((()))))
  (check (equal? (run* () (=== 1 2))
                 '())))

(define-test var-bindings
  (check (equal? (run* (p q) (=== p q))
                 '(((_.0 _.0))))))

(define-test triangular-substitution
  (check (equal? (run* (p q r) (=== (list p q) (list q r)))
                 '(((_.0 _.0 _.0))))))

(define-test dual-pair
  (check (equal? (run* (p q) (=== (cons p 1) (cons 2 q)))
                 '(((2 1))))))

(define-test deeper-non-occurs-check-check
  (check (equal? (run* (p)
                   (fresh (q r)
                     (=== q (cons 1 r))
                     (=== r (cons 1 p))
                     (=== p (cons 1 2))))
                 '((((1 . 2)))))))
