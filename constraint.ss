#!r6rs

(library (chrKanren constraint)
  (export
   define-constraint
          constraint make-constraint constraint?
          constraint-constructor constraint-reifier constraint-operands

          <- scheme assignment? scheme-check?)
  (import (rnrs)
          (chrKanren utils)
          (chrKanren goals)
          (srfi :39 parameters))

  ;; TODO, make constraint operators into goal and remove `posting` hack
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

  (define-syntax-rule (define-constraint (constructor . arglist) reifier-body ...)
    (define (constructor . arglist)
      (post (make-constraint constructor
                             (lambda arglist reifier-body ...)
                             (dotted-list arglist)))))

  (define-constraint (<- vr val)
    (error '<- "Should never reify"))
  (define-constraint (scheme pred obj . objs)
    (error 'scheme "Should never reify"))

  (define (assignment? con)
    (and (constraint? con)
         (eq? (constraint-constructor con) <-)))

  (define (scheme-check? con)
    (and (constraint? con)
         (eq? (constraint-constructor con) scheme))))
