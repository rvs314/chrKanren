#!r6rs

(library (chrKanren state)
  (export state empty-state state-subst state-subst-update state-subst-set)
  (import (rnrs) (chrKanren subst))

  (define-record-type state
    (fields subst))

  (define empty-state (make-state empty-subst))

  (define (state-subst-update state proc)
    (make-state (proc (state-subst state))))

  (define (state-subst-set _state subst^)
    (make-state subst^)))
