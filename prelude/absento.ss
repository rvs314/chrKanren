#!r6rs

(library (chrKanren prelude absento)
  (export absento)
  (import (rnrs)
          (chrKanren rule)
          (chrKanren goals)
          (chrKanren compare)
          (chrKanren vars)
          (chrKanren varmap)
          (chrKanren utils)
          (chrKanren unify)
          (chrKanren prelude disunification)
          (chrKanren prelude types)
          (chrKanren syntax))

  (define-constraint (absento needle haystack)
    `(absento ,needle ,haystack))

  ;; TODO: This is begin copied verbatim from `state.ss`; factor out
  (define (free-variables obj)
    (cond
      [(var? obj) (list obj)]
      [(pair? obj) (append (free-variables (car obj)) (free-variables (cdr obj)))]
      [else '()]))

  (define (trivial-instantiation? vs l)
    (define rs (free-variables vs))
    (find-subtree (lambda (obj) (and (var? obj) (not (memq obj rs))))
                  l))

  (define-rules
    (forall (x)
      (absento x x)
      <=>
      fail)
    (forall (x y)
      (absento x y)
      (ground (negate pair?) y)
      <=>
      (=/= x y))
    (forall (x y)
      (absento x y)
      (absento x y)
      <=>
      (absento x y))
    (forall (x y n p)
      (absento x y)
      (typeo y n p)
      <=>
      (typeo y n p)
      (=/= x y))
    (forall (x y z)
      (absento x (cons y z))
      <=>
      (=/= x (cons y z))
      (absento x y)
      (absento x z))
    (forall (x y vs)
      (reifying vs)
      (absento x y)
      (scheme
       trivial-instantiation?
       vs
       y)
      <=>
      (reifying vs))
    (forall (x y vs)
      (reifying vs)
      (absento x y)
      (scheme
       trivial-instantiation?
       vs
       x)
      <=>
      (reifying vs))
    (forall (x y vs)
      (reifying vs)
      (absento x y)
      (scheme (lambda (y x) (subterm? y x empty-varmap)) y x)
      <=>
      (reifying vs))
    (forall (x y vs)
      (reifying vs)
      (absento x y)
      (=/=* (list (cons x y)))
      <=>
      (reifying vs)
      (absento x y))
    (forall (x y vs)
      (reifying vs)
      (absento x y)
      (=/=* (list (cons y x)))
      <=>
      (reifying vs)
      (absento x y))
    (forall (x y z vs)
      (reifying vs)
      (absento x y)
      (absento z y)
      (ground (lambda (x y) (subterm? x y empty-varmap)) x z)
      <=>
      (reifying vs)
      (absento x y))
    (forall (x y ls vs)
      (reifying vs)
      (absento x y)
      (=/=* ls)
      (ground subsumes? (list (cons x y)) ls)
      <=>
      (reifying vs)
      (absento x y))))
