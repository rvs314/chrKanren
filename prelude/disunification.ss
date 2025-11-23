#!r6rs

(library (chrKanren prelude disunification)
  (export =/=)
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
          (chrKanren prelude types))

  (define-constraint (=/= _l _r) (error '=/= "Should not reify =/="))
  (define-constraint (=/=* alist)
    `(=/= ,@(lex-sort (map (compose lex-sort pair->tuple) alist))))

  (define (reducible? diseq-spec)
    (check (list? diseq-spec))
    (exists (lambda (aq) (not (var? (car aq))))
            diseq-spec))

  (define (reduced diseq-spec)
    (define u
     (unify (map car diseq-spec)
            (map cdr diseq-spec)
            empty-varmap))
    (and u (varmap->alist u)))

  (define (subsumes? ls rs)
    (define (equal-or-lauqe? l r)
      (or (equal? l r)
          (equal? l (cons (cdr r) (car r)))))
    (lset<= equal-or-lauqe? ls rs))

  ;; TODO: This is copied verbatim from `state.ss`; factor out
  (define (free-variables obj)
    (cond
      [(var? obj) (list obj)]
      [(pair? obj) (append (free-variables (car obj)) (free-variables (cdr obj)))]
      [else '()]))

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
    (forall (l r rs)
      (=/=* (cons (cons l r) rs))
      (numbero l)
      (symbolo r)
      <=>
      (numbero l)
      (symbolo r))
    (forall (l r rs)
      (=/=* (cons (cons l r) rs))
      (numbero r)
      (symbolo l)
      <=>
      (numbero r)
      (symbolo l))
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
      (let ([ls (reduced ls)])
        (if ls
            (=/=* ls)
            succeed))
      (reifying vs))
    (forall (ls vs rs)
      (reifying vs)
      (=/=* ls)
      (=/=* rs)
      (ground (lambda (ls rs) (lset= equal? ls rs)) ls rs)
      <=>
      (reifying vs)
      (=/=* ls))
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
    (forall (ls o n p vs)
       (reifying vs)
       (=/=* ls)
       (typeo o n p)
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
       (typeo o n p))))
