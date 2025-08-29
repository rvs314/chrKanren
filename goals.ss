#!r6rs

(library (chrKanren goals)
  (export              goal        goal?
          conj         conjunction conjunction? conjunction-left conjunction-right
          disj         disjunction disjunction? disjunction-left disjunction-right
          make-call    call        call?        call-target call-arguments
          fail         failure     failure?
          succeed      success     success?
          with-values  projection  projection? projection-vars projection-cont
          Zzz          delay       delay?      delay-cont)

  (import (rnrs)
          (only (srfi :1) reduce)
          (chrKanren subst) (chrKanren vars) (chrKanren state) (chrKanren utils))

  (define-record-type goal)

  (define-record-type conjunction
    (parent goal)
    (fields left right))

  (define (conj . cs)
    (reduce make-conjunction succeed cs))

  (define-record-type disjunction
    (parent goal)
    (fields left right))

  (define (disj . cs)
    (reduce make-disjunction fail cs))

  (define-record-type success
    (parent goal))

  (define succeed (make-success))

  (define-record-type failure
    (parent goal))

  (define fail (make-failure))

  (define-record-type call
    (parent goal)
    (sealed #t)
    (fields target arguments))

  (define-record-type projection
    (parent goal)
    (sealed #t)
    (fields vars cont))

  (define-syntax-rule (with-values (vs ...)
                        body ...)
    (make-projection
     (list vs ...)
     (lambda (vs ...)
       body ...)))

  (define-record-type delay
    (parent goal)
    (sealed #t)
    (fields cont))

  (define-syntax-rule (Zzz body ...)
    (make-delay (lambda () body ...))))
