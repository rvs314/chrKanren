#!r6rs

(import (rnrs) (chrKanren utils) (chrKanren base) (chrKanren test))

(*finite-maturation-limit* 1000)

(define-constraint (loop^o x))

(define-rules
  (forall (x)
     (forget (loop^o x))
     <=>
     (loop^o x)))

(define-relation (loopo x)
  (loopo x))

(define-syntax-rule (try body ...)
  (guard (vr [else vr])
    body ...))

(define-test interleave-search
  (check condition?
         (try (run-finite 1 (q) (loopo q))))
  (check (equal? (run-finite 1 (q)
                             (disj (loopo q)
                                   (=== q 1)))
                 '(((1))))))

(define-test interleave-search-and-constraints
  (check condition?
         (try (run-finite 1 (q) (loop^o q))))
  (check (equal? (run-finite 1 (q)
                             (disj (loop^o q)
                                   (=== q 1)))
                 '(((1))))))
