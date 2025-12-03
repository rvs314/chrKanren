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
      (check (eq? p (varmap-lookup p s)))
      (check (eq? p (varmap-lookup q s))))))

(define-test triangular-substitution
  (fresh (p q r)
    (let* ([s (unify (list p q) (list q r) empty-varmap)])
      (check (eq? p (varmap-lookup p s)))
      (check (eq? p (varmap-lookup q s)))
      (check (eq? p (varmap-lookup r s))))))

(define-test dual-pair
  (fresh (p q)
    (let* ([s (unify `(,p . 1) `(2 . ,q) empty-varmap)])
      (check (equal? 2 (varmap-lookup p s)))
      (check (equal? 1 (varmap-lookup q s))))))

(define-test vector-unification
  (fresh (p q)
    (let* ([s (unify `#(,p 1) `#(2 ,q) empty-varmap)])
      (check (equal? 2 (varmap-lookup p s)))
      (check (equal? 1 (varmap-lookup q s))))))

(define-test walk*?
  (define a (make-var 'a))
  (define b (make-var 'b))

  (check (equal? `(a: ,a b: ,b) (walk*  `(a: ,a b: ,b) empty-varmap)))
  (check (equal? `#(a: ,a b: ,b) (walk*  `#(a: ,a b: ,b) empty-varmap)))
  (check (equal? '(a: 3 b: 4)
                 (walk* `(a: ,a b: ,b) (varmap-extend a 3 (varmap-extend b 4 empty-varmap)))))
  (check (equal? `(a: 3 b: ,b) (walk* `(a: ,a b: ,b) (varmap-extend a 3 empty-varmap))))
  (check (equal? 3 (walk b (varmap-extend a 3 (varmap-extend b a empty-varmap))))))
