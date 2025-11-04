#!r6rs

(library (chrKanren state)
  (export empty-state state?
          constrain query)
  (import (rnrs)
          (only (srfi :1 lists) filter-map)
          (chrKanren utils)
          (chrKanren check)
          (chrKanren goals)
          (chrKanren constraint)
          (chrKanren streams)
          (chrKanren unify)
          (chrKanren vars)
          (chrKanren varmap))

  (define-record-type state
    (fields subst facts))

  (define empty-state
    (make-state empty-varmap '()))

  ;; State -> (or Constraint (Listof Constraint)) -> Stream
  (define (constrain state cs)
    (let ([cs (if (constraint? cs) (list cs) cs)])
      (check (state? state))
      (check (for-all constraint? cs))

      (let*-values ([(assignments constraints)
                     (partition assignment? cs)]
                    [(facts) (append constraints (state-facts state))]
                    [(subst) (varmap-extend-all (map
                                                 (lambda (as)
                                                   (cons (car (constraint-operands as))
                                                         (cadr (constraint-operands as))))
                                                 assignments)
                                                (state-subst state))]
                    [(state1) (make-state subst facts)])
        (propagate-constraints state1))))

  (define (unify-constraint lcon rcon vmap var?)
    (and (eq? (constraint-constructor lcon)
              (constraint-constructor rcon))
         (unify (constraint-operands lcon)
                (constraint-operands rcon)
                vmap
                var?)))

  (define (rule-var? rule)
    (check (rule? rule))
    (lambda (vr)
      (memq vr (rule-free-variables rule))))

  ;; Factset -> Varmap -> Constraint -> List Varmap
  (define (query-constraint facts vmap constraint var?)
    (check (constraint? constraint))
    ;; TODO: add `var?` primitive functor
    ;; TODO: check for invalid assignment functor
    (if (eq? (constraint-constructor constraint) ground)
        (let-values ([(pred obj1 objs) (apply values (constraint-operands constraint))])
          (and (not (exists var? (cons obj1 objs)))
               (apply pred obj1 objs)
               vmap))
        (filter (lambda (con)
                  (unify-constraint con
                                    constraint
                                    vmap
                                    var?))
                facts)))

  ;; Rule -> Varmap -> List Consequence
  (define (instantiate-consequences rule vmap)
    (if (find failure? (rule-consequences rule))
        (list fail)
        (filter-map
         (lambda (con)
           (and (posting? con)
                (let ([con (posting-constraint con)])
                  (make-constraint
                   (constraint-constructor con)
                   (walk* (constraint-operands con) vmap (rule-var? rule))))))
         (rule-consequences rule))))

  ;; State -> Rule -> Listof Consequence
  (define (apply-rule state rule)
    (define (find-assignment vmap prereqs)
      (check (list? prereqs))

      (if (null? prereqs)
          vmap
          (let* ([prereq (car prereqs)]
                 [ps (query-constraint (state-facts state)
                                       vmap
                                       (posting-constraint prereq)
                                       (rule-var? rule))])
            (exists (lambda (as) (find-assignment as (cdr prereqs))) ps))))
    (check (state? state))
    (check (rule? rule))

    (let ([as (find-assignment (state-subst state) (rule-prereqs rule))])
      (if as
          (instantiate-consequences rule as)
          '())))

  ;; State -> Stream
  (define (propagate-constraints state)
    (check (state? state))
    (or (exists (lambda (rule)
                  (check (rule? rule))
                  (let ([ap (apply-rule state rule)])
                   (cond
                     [(exists failure? ap) empty-stream]
                     [(null? ap) #f]
                     [else (constrain state ap)])))
                (*constraint-handling-rules*))
        (make-singleton state)))

  ;; State -> Listof Var -> (Values (Listof Term) (Listof Constraint))
  (define (query state arguments)
    (define (relevant? con)
      (exists (lambda (arg)
                (memq arg (constraint-operands con)))
              arguments))

    (check (state? state))
    (check (list? arguments))
    (check (for-all var? arguments))

    (values (walk* arguments (state-subst state))
            (filter relevant? (state-facts state)))))
