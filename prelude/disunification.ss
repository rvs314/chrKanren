#!r6rs

(library (chrKanren prelude disunification)
  (export =/= =/=* subsumes?)
  (import (rnrs)
          (only (srfi :1 lists) filter-map lset<= lset=)
          (chrKanren syntax)
          (chrKanren check)
          (chrKanren utils)
          (chrKanren vars)
          (chrKanren goals)
          (chrKanren unify)
          (chrKanren varmap)
          (srfi :2 and-let*)
          (chrKanren compare)
          (chrKanren rule)
          (chrKanren prelude unification)
          (chrKanren prelude types))

  (define-constraint (=/= _l _r) (error '=/= "Should not reify =/="))
  (define-constraint (=/=* alist)
    `(=/= ,@(lex-sort (map (compose lex-sort pair->tuple) alist))))

  (define (reducible? diseq-spec)
    (not (equal? (reduced diseq-spec) diseq-spec)))

  (define (reduced diseq-spec)
    (false-map (compose varmap->alist square-varmap) (unify* diseq-spec empty-varmap)))

  (define (subsumes? ls rs)
    (unify* ls (alist->varmap rs) (const #f)))

  (define (trivial-instantiation? vs l)
    (define rs (free-variables vs))
    (find-subtree (lambda (obj) (and (var? obj) (not (memq obj rs))))
                  l))

  ;; (=/= ((A . B) (C . D) (E . F)))
  ;; Means either, A ≠ B or C ≠ D or E ≠ F

  (define-rules
    (forall (x y) (=/= x y) <=> (=/=* (list (cons x y))))
    (forall () (=/=* (list)) <=> fail)
    (forall (l ls) (=/=* (cons (cons l l) ls)) <=> (=/=* ls))
    (forall (l r rs)
      (=/=* (cons (cons l r) rs))
      (ground procedure? l)
      (ground procedure? r)
      (ground (negate eq?) l r)
      <=>
      succeed)
    (forall (l r rs)
      (=/=* (cons (cons l r) rs))
      (ground atom? l)
      (ground (negate equal?) l r)
      <=>
      succeed)
    (forall (l r rs)
      (=/=* (cons (cons l r) rs))
      (ground atom? r)
      (ground (negate equal?) l r)
      <=>
      succeed)
    (forall (l r rst n p pr)
      (typeo l n p pr)
      (=/=* (cons (cons l r) rst))
      (ground (lambda (p r) (not (p r))) p r)
      <=>
      (typeo l n p pr))
    (forall (l r rst n p pr)
      (typeo r n p pr)
      (=/=* (cons (cons l r) rst))
      (ground (lambda (p l) (not (p l))) p l)
      <=>
      (typeo r n p pr))
    (forall (l r n n^ p p^ rs pr pr^)
      (=/=* (cons (cons l r) rs))
      (typeo l n p pr)
      (typeo r n^ p^ pr^)
      (ground (negate eq?) n n^)
      <=>
      (typeo l n p pr)
      (typeo r n^ p^ pr^))
    (forall (ll lr rl rr rs)
      (=/=* (cons (cons (cons ll lr) (cons rl rr)) rs))
      <=>
      (=/=* (cons* (cons ll rl) (cons lr rr) rs)))
    (forall (ls rs vs)
      (reifying vs)
      (=/=* ls)
      (=/=* rs)
      (ground subsumes? ls rs)
      <=>
      (reifying vs)
      (=/=* ls))
    (forall (ls vs)
      (reifying vs)
      (=/=* ls)
      (ground reducible? ls)
      <=>
      (or (false-map =/=* (reduced ls))
          succeed)
      (reifying vs))
    (forall (ls vs)
      (reifying vs)
      (=/=* ls)
      (ground
       (lambda (vs ls)
         (exists
          (lambda (l) (trivial-instantiation? vs l))
          ls))
       vs
       ls)
      <=>
      (reifying vs))
    (forall (ls o n p pr vs)
      (reifying vs)
      (=/=* ls)
      (typeo o n p pr)
      (scheme (lambda (o ls p)
                (exists
                 (lambda (ac)
                   (and (not (var? (cdr ac)))
                        (eq? o (car ac))
                        (not (p (cdr ac)))))
                 ls))
              o
              ls
              p)
      <=>
      (reifying vs)
      (typeo o n p pr))))
