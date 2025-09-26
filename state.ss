#!r6rs

(library (chrKanren state)
  (export state make-state empty-state state?
          state-subst state-subst-update state-subst-set
          state-lookup state-extend
          state->goal state=?
          reify)
  (import (rnrs) (chrKanren utils) (chrKanren subst))

  (define-record-type state
    (fields subst))

  (define empty-state (make-state empty-subst))

  (define (state-subst-set _state subst^)
    (make-state subst^))

  (define (state-subst-update state proc)
    (state-subst-set state (proc (state-subst state))))

  (define (state-extend key value state)
    (define subst^ (extend key value (state-subst state)))
    (and subst^
         (state-subst-set state subst^)))

  (define (state-lookup key state)
    (lookup key (state-subst state)))

  (define (state->goal state)
    (subst->goal (state-subst state)))

  (define (state=? state1 state2)
    (subst=? (state-subst state1) (state-subst state2)))

  (define (reify vars state)
    (map (lambda (v) (walk* v (state-subst state))) vars)))
