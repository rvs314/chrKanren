#!r6rs

(library (chrKanren prelude absento)
  (export absento)
  (import (rnrs)
          (chrKanren rule)
          (chrKanren goals)
          (chrKanren compare)
          (chrKanren vars)
          (chrKanren utils)
          (chrKanren unify)
          (chrKanren prelude unification)
          (chrKanren prelude disunification)
          (chrKanren prelude types)
          (chrKanren syntax))

  (define-constraint (absento needle haystack)
    `(absento ,needle ,haystack))

  ;; True iff there is a variable in `term`
  ;; which is not a member of `relevant-variables`
  (define (any-irrelevant? relevant-variables term)
    (define relevant (free-variables relevant-variables))
    (define (relevant? v) (memq v relevant))
    (find (negate relevant?) (free-variables term)))

  (define-rules
    (forall (x)
      (absento x x)
      =>
      fail)
    (forall (x y)
      (absento x y)
      (forget (absento x y)))
    (forall (x y)
      (forget (absento x y))
      (ground atom? y)
      =>
      (=/= x y))
    (forall (x y n p)
      (forget (absento x y))
      (typeo y n p #f)
      =>
      (=/= x y))
    (forall (x y z)
      (forget (absento x (cons y z)))
      =>
      (=/= x (cons y z))
      (absento x y)
      (absento x z))
    (forall (x y vs)
      (reifying vs)
      (forget (absento x y))
      (ground list? vs)
      (scheme (lambda (vs x y)
                (any-irrelevant? vs (cons x y)))
              vs
              x
              y))
    (forall (x y vs)
      (reifying vs)
      (forget (absento x y))
      (scheme (lambda (y x)
                (subterm? y x empty-varmap))
              y
              x))
    (forall (x y z vs)
      (reifying vs)
      (absento x y)
      (forget (absento z y))
      (ground (lambda (x z) (subterm? x z empty-varmap)) x z))
    (forall (x y ls vs)
      (reifying vs)
      (absento x y)
      (forget (=/=* ls))
      (ground subsumes? (list (cons x y)) ls))))
