#!r6rs

(library (chrKanren state)
  (export history-entry?
          empty-state state state?
          make-state
          state=? state-facts state-subst state-hist
          state-map-subst state-map-facts state-map-hist
          state-with-subst state-with-facts state-with-hist
          state-seen-application? state-record-application)
  (import (rnrs)
          (only (srfi :1 lists) filter-map lset-difference)
          (srfi :2 and-let*)
          (srfi :26 cut)
          (rnrs mutable-pairs)
          (chrKanren utils)
          (chrKanren check)
          (chrKanren goals)
          (chrKanren rule)
          (chrKanren streams)
          (chrKanren unify)
          (chrKanren vars)
          (chrKanren hashmap))

  (define history-entry? (listof constraint?))
  (define history-entries? (listof history-entry?))
  (define-hashmap-type history rule? history-entries? rule-id)

  (define-record-type state
    (fields subst facts hist)
    (protocol
     (lambda (new)
       (define-check
         (make-state [subst varmap?]
                     [facts (listof constraint?)]
                     [hist  history?])

         state?
         (new subst facts hist))
       make-state)))

  ;; Helper for functional updates of state object.
  ;; At some point in the future, we should write a library
  ;; that generates this code automatically using static reflection
  ;; - Raffi

  (define (state-map proc state)
    (call-with-values
     (lambda ()
       (proc (state-subst state)
             (state-facts state)
             (state-hist state)))
     (case-lambda
       [(subst^ facts^ hist^) (make-state subst^ facts^ hist^)]
       [(obj) (if (state? obj) obj state)]
       [_ state])))

  (define (state-map-subst proc state)
    (state-map (lambda (s f h) (values (proc s) f h)) state))

  (define (state-map-facts proc state)
    (state-map (lambda (s f h) (values s (proc f) h)) state))

  (define (state-map-hist proc state)
    (state-map (lambda (s f h) (values s f (proc h))) state))

  (define (state-map->state-with state-map-obj)
    (lambda (obj state)
      (state-map-obj (const obj) state)))

  (define state-with-subst (state-map->state-with state-map-subst))
  (define state-with-facts (state-map->state-with state-map-facts))
  (define state-with-hist  (state-map->state-with state-map-hist))

  (define (same-history-entry? cs1 cs2)
    (and (= (length cs1) (length cs2))
         (for-all (on eqv? constraint-id) cs1 cs2)))

  (define-check (state-seen-application?
                 [state state?]
                 [rule rule?]
                 [constraints (listof constraint?)])
    any?
    (and (pair? constraints)
         (let ([entries (hashmap-ref (state-hist state)
                                     rule
                                     (lambda () '()))])
           (and (pair? entries)
                (find (cut same-history-entry? <> constraints)
                      entries)))))

  (define (state-record-application state rule witnesses)
    (define (constraint-to-be-removed witness)
      (and (witness-removed? witness)
           (posting-constraint (witness-constraint witness))))
    (define constraints (map (compose posting-constraint
                                      witness-constraint)
                             witnesses))
    (define constraints-to-be-removed
      (filter-map constraint-to-be-removed witnesses))
    (state-map
     (lambda (subst facts hist)
       (define facts^
         (lset-difference eq? facts constraints-to-be-removed))
       (define existing (hashmap-ref hist rule (lambda () '())))
       (define hist^
         (hashmap-set hist
                      rule
                      (cons constraints existing)))
       (values subst facts^ hist^))
     state))

  (define empty-state
    (make-state empty-varmap '() empty-history))

  (define state=?
    (conjoin (on and-proc state?)
             (on equal? state-subst)
             (on equal? state-facts)
             (on equal? state-hist))))
