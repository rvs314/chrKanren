#!r6rs

(library (chrKanren goals)
  (export
   goal goal? goal=?

   define-constraint
   constraint make-constraint constraint?
   constraint-constructor constraint-reifier constraint-operands

   === scheme same-check? scheme-check? reifying

   conjunction conj conjunction? conjunction-left conjunction-right
   disj disjunction disjunction? disjunction-left disjunction-right

   call make-call call? call-target call-arguments

   fail         failure     failure?
   succeed      success     success?

   posting post posting? posting-constraint

   Zzz delay delay? delay-cont)

  (import (rnrs)
          (only (srfi :1) reduce-right)
          (chrKanren check)
          (chrKanren utils))

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

  (define-record-type (posting post posting?)
    (parent goal)
    (sealed #t)
    (fields constraint))

  (define-record-type delay
    (parent goal)
    (sealed #t)
    (fields cont))

  (define-syntax-rule (Zzz body ...)
    (make-delay (lambda () body ...)))

  (define-record-type constraint
    (fields constructor reifier operands))

  (define-syntax dotted-list-helper
    (syntax-rules ()
      [(dotted-list-helper (acc ...) ())
       (list acc ...)]
      [(dotted-list-helper (acc ...) (a . b))
       (dotted-list-helper (acc ... a) b)]
      [(dotted-list-helper (acc ...) b)
       (cons* acc ... b)]))

  (define-syntax dotted-list
    (syntax-rules ()
      [(dotted-list foo)
       (dotted-list-helper () foo)]))

  (define-syntax-rule (define-constraint (constructor . arglist) body ...)
    (define (constructor . arglist)
      (post (make-constraint constructor
                             (lambda arglist body ...)
                             (dotted-list arglist)))))

  (define-constraint (=== lhs rhs)
    (error '=== "This should never reify"))
  (define-constraint (scheme pred obj . objs)
    (error 'scheme "This should never reify"))
  (define-constraint (reifying query-variables)
    #f)

  (define (same-check? con)
    (and (constraint? con)
         (eq? (constraint-constructor con) ===)))

  (define (scheme-check? con)
    (and (constraint? con)
         (eq? (constraint-constructor con) scheme)))

  (define constraint=?
    (conjoin
     (on and-proc constraint?)
     (on eq? constraint-constructor)
     (on equal? constraint-operands)))

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
       (on and-proc call?)
       (on equal? call-arguments)
       (on eq? call-target))
      (conjoin
       (on and-proc posting?)
       (on constraint=? posting-constraint))
      (conjoin
       (on and-proc delay?)
       (on goal=? (lambda (x) (x)) delay-cont))))))
