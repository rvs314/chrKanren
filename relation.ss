#!r6rs

(library (chrKanren relation)
  (export relation relation? relation-name relation-args relation-builder
          make-relation call-relation)
  (import (rnrs) (chrKanren vars) (chrKanren subst) (chrKanren goals) (chrKanren utils))

  (define-record-type relation
    (fields name args builder))

  (define (call-relation rel . args)
    (apply (relation-builder rel) args)))
