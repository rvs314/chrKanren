#!r6rs

(library (chrKanren goals)
  (export
   goal goal? goal=?

   conjunction conj conjunction? conjunction-left conjunction-right
   disj disjunction disjunction? disjunction-left disjunction-right

   make-call call call? call-target call-arguments

   fail         failure     failure?
   succeed      success     success?

   == unification unification? unification-lhs unification-rhs

   Zzz delay delay? delay-cont)

  (import (rnrs)
          (only (srfi :1) reduce-right)
          (chrKanren check)
          (chrKanren vars) (chrKanren utils))

  (define-record-type goal)

  (define-record-type conjunction
    (parent goal)
    (fields left right)
    (protocol
     (lambda (new)
       (lambda (l r)
         (check (goal? l))
         (check (goal? r))
         ((new) l r)))))

  (define (conj . cs)
    (reduce-right make-conjunction succeed cs))

  (define-record-type disjunction
    (parent goal)
    (fields left right)
    (protocol
     (lambda (new)
       (lambda (l r)
         (check (goal? l))
         (check (goal? r))
         ((new) l r)))))

  (define (disj . cs)
    (reduce-right make-disjunction fail cs))

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

  (define-record-type (unification == unification?)
    (parent goal)
    (sealed #t)
    (fields lhs rhs))

  (define-record-type delay
    (parent goal)
    (sealed #t)
    (fields cont))

  (define-syntax-rule (Zzz body ...)
    (make-delay (lambda () body ...)))

  (define goal=?
    (eta
     (disjoin
      (on and-proc success?)
      (on and-proc failure?)
      (conjoin
       (on and-proc conjunction?)
       (on goal=? conjunction-left)
       (on goal=? conjunction-right))
      (conjoin
       (on and-proc disjunction?)
       (on goal=? disjunction-left)
       (on goal=? disjunction-right))
      (conjoin
       (on and-proc unification?)
       (on equal? unification-lhs)
       (on equal? unification-rhs))
      (conjoin
       (on and-proc call?)
       (on equal? call-arguments)
       (on eq? call-target))
      (conjoin
       (on and-proc delay?)
       (on goal=? (lambda (x) (x)) delay-cont))))))
