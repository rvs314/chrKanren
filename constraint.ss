#!r6rs

(library (chrKanren constraint)
  (export declare-constraint
          constraint make-constraint constraint?
          constraint-constructor constraint-operands

          <- scheme assignment? scheme-check? debug debug?

          *constraint-handling-rules* define-constraint-handling-rules
          forall
          rule rule? rule-free-variables rule-prereqs rule-consequences)
  (import (rnrs)
          (chrKanren utils)
          (chrKanren goals)
          (chrKanren vars)
          (srfi :39 parameters))

  (define-record-type rule
    (fields free-variables prereqs consequences))

  ;; TODO, make constraint operators into goal and remove `posting` hack
  (define-record-type constraint
    (fields constructor operands))

  (define *constraint-handling-rules* (make-parameter '()))

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

  (define-syntax-rule (declare-constraint (constructor . arglist) ...)
    (begin (define (constructor . arglist)
             (post (make-constraint constructor (dotted-list arglist))))
           ...))

  (declare-constraint
   (<- vr val)
   (scheme pred obj . objs)
   (debug . xs))

  (define (assignment? con)
    (and (constraint? con)
         (eq? (constraint-constructor con) <-)))

  (define (scheme-check? con)
    (and (constraint? con)
         (eq? (constraint-constructor con) scheme)))

  (define (debug? con)
    (and (constraint? con)
         (eq? (constraint-constructor con) debug)))

  (define-syntax forall (syntax-rules ()))

  (define-syntax parse-rule
    (syntax-rules (forall =>)
      [(parse-rule (vs ...) (gl ...) (=> res ...))
       (fresh (vs ...)
         (make-rule (list vs ...) (list gl ...) (list res ...)))]
      [(parse-rule (vs ...) (gl ...) (g₀ rest ...))
       (parse-rule (vs ...) (gl ... g₀) (rest ...))]
      [(parse-rule (forall (vs ...) rest ...))
       (parse-rule (vs ...) () (rest ...))]))

  (define-syntax-rule (define-constraint-handling-rules rule ...)
    (*constraint-handling-rules*
     (cons* (parse-rule rule) ... (*constraint-handling-rules*)))))
