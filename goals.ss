#!r6rs

(library (chrKanren goals)
  (export
   goal goal? goal=?

   define-constraint
   constraint-id
   constraint make-constraint constraint?
   constraint-constructor constraint-reifier constraint-operands

   === scheme same-check? scheme-check? reifying constraint-check?

   conjunction conj conjunction? conjunction-left conjunction-right
   disj disjunction disjunction? disjunction-left disjunction-right

   call make-call call? call-target call-arguments

   fail         failure     failure?
   succeed      success     success?

   posting posting-id post posting? posting-constraint

   Zzz delay delay? delay-cont)

  (import (rnrs)
          (only (srfi :1) reduce-right)
          (srfi :39 parameters)
          (chrKanren check)
          (chrKanren utils))

  (define-record-type goal)

  (define-record-type conjunction
    (parent goal)
    (fields left right)
    (protocol
     (lambda (new)
       (lambda-check ([l goal?] [r goal?])
         conjunction?
         ((new) l r)))))

  (define (conj . cs)
    (reduce-right make-conjunction succeed cs))

  (define-record-type disjunction
    (parent goal)
    (fields left right)
    (protocol
     (lambda (new)
       (lambda-check ([l goal?] [r goal?])
         disjunction?
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
    (fields id constraint)
    (protocol
     (let ([counter 0])
       (lambda (new)
         (lambda-check ([con constraint?])
           posting?
           (set! counter (+ 1 counter))
           ((new) counter con))))))

  (define-record-type delay
    (parent goal)
    (sealed #t)
    (fields cont))

  (define-syntax-rule (Zzz body ...)
    (make-delay (named-lambda (delayed) body ...)))

  (define-record-type constraint
    (fields id constructor reifier operands)
    (protocol
     (let ([counter 0])
       (lambda (new)
         (named-lambda-check
             (make-constraint
              [con procedure?]
              [ref procedure?]
              [ops any?])
           constraint?
           (set! counter (+ 1 counter))
           (new counter con ref ops))))))

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

  (define-syntax define-constraint
    (syntax-rules ()
      [(define-constraint (constructor . arglist))
       (define-constraint (constructor . arglist)
         (cons 'constructor (list . arglist)))]
      [(define-constraint (constructor . arglist)
         body ...)
       (define (constructor . arglist)
         (post (make-constraint constructor
                                (lambda arglist body ...)
                                (dotted-list arglist))))]))

  (define-constraint (=== lhs rhs)
    (error '=== "=== should never reify" lhs rhs))
  (define-constraint (scheme pred obj . objs)
    (apply error
           'scheme
           "The `scheme` constraint should never reify"
           pred
           obj
           objs))
  (define-constraint (reifying _query-variables)
    #f)

  (define (same-check? con)
    (and (constraint? con)
         (eq? (constraint-constructor con) ===)))

  (define (scheme-check? con)
    (and (constraint? con)
         (eq? (constraint-constructor con) scheme)))

  (define (constraint-check? con)
    (and (constraint? con)
         (not (memq (constraint-constructor con) (list === scheme)))))

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
