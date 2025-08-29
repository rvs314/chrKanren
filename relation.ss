#!r6rs

(library (chrKanren relation)
  (export relation relation? relation-name relation-args relation-builder
          define-relation)
  (import (rnrs) (chrKanren vars) (chrKanren subst) (chrKanren goals) (chrKanren utils))

  (define-record-type relation
    (fields name args builder))

  (define-syntax-rule (define-relation (name arg ...)
                        body body* ...)
    (define the-relation
      (make-relation
       'name
       (list 'arg ...)
       (lambda (arg ...)
         (conj body body* ...))))

    (define (name arg ...)
      (Zzz (make-call the-relation (list arg ...))))))
