#!r6rs

(library (chrKanren prelude absento)
  (export absento)
  (import (rnrs)
          (chrKanren rule)
          (chrKanren goals)
          (chrKanren compare)
          (chrKanren vars)
          (chrKanren utils)
          (chrKanren prelude disunification)
          (chrKanren prelude types)
          (chrKanren syntax))

  (define-constraint (absento needle haystack)
    `(absento ,needle ,haystack))

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
      (reifying vs))))
