#!r6rs

(import (rnrs)
        (chrKanren test)
        (chrKanren check)
        (chrKanren vars)
        (chrKanren varmap))

(define a (make-var 'a))
(define b (make-var 'b))

(define lookup varmap-lookup)
(define extend varmap-extend)
(define empty-subst empty-varmap)

(define-test Simple-extend/lookup
  (check (equal? a (lookup a empty-subst)))
  (check (extend a 3 empty-varmap))
  (check (equal? 3 (lookup a (extend a 3 empty-subst))))
  (check (equal? 3 (lookup a (extend a 3 (extend a 2 empty-subst)))))
  (check (equal? b (lookup a (extend a b (extend a 2 empty-subst))))))
