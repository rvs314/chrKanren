#!r6rs

(library (chrKanren constraint)
  (export
   declare-constraint
          constraint make-constraint constraint?
          constraint-constructor constraint-operands

          <- scheme assignment? scheme-check? debug debug?)
  (import (rnrs)
          (chrKanren utils)
          (chrKanren goals)
          (srfi :39 parameters))

  ;; TODO, make constraint operators into goal and remove `posting` hack
  (define-record-type constraint
    (fields constructor operands))

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
         (eq? (constraint-constructor con) debug))))
