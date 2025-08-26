#!r6rs

(library (chrKanren goals)
  (export              goal        goal?
          conj         conjunction conjunction? conjuncts
          disj         disjunction disjunction? disjuncts
          make-call    call        call?        call-target call-arguments
          fail                     failure?
          succeed                  success?
          with-values  projection  projection? projection-vars projection-cont
          Zzz          delay       delay?      delay-cont)

  (import (rnrs) (chrKanren subst) (chrKanren vars) (chrKanren state) (chrKanren utils))

  (define-record-type goal)

  (define (fan-out new) (lambda args ((new) args)))

  (define-record-type (conjunction conj conjunction?)
    (parent goal)
    (fields (immutable children conjuncts))
    (sealed #t)
    (protocol fan-out))

  (define-record-type (disjunction disj disjunction?)
    (parent goal)
    (fields (immutable children disjuncts))
    (sealed #t)
    (protocol fan-out))

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
    (make-delay (lambda () body ...)))

  (define fail (disj))
  (define (failure? obj)
    (and (disjunction? obj) (null? (disjuncts obj))))

  (define succeed (conj))
  (define (success? obj)
    (and (conjunction? obj) (null? (conjuncts obj)))))
