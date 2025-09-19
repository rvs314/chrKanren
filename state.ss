#!r6rs

(library (chrKanren state)
  (export state make-state empty-state state?
          state-subst state-subst-update state-subst-set state-extend
          state->goal)
  (import (rnrs) (chrKanren subst))

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

  (define (state->goal state)
    (subst->goal (state-subst state))))
