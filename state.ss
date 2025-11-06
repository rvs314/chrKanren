#!r6rs

(library (chrKanren state)
  (export empty-state state?
          constrain make-state query
          state=?)
  (import (rnrs)
          (only (srfi :1 lists) filter-map)
          (chrKanren utils)
          (chrKanren check)
          (chrKanren goals)
          (chrKanren rule)
          (chrKanren streams)
          (chrKanren unify)
          (chrKanren vars)
          (chrKanren varmap))

  (define-record-type state
    (fields subst facts)
    (protocol
     (lambda (new)
       (lambda (subst facts)
         ;; TODO: make an abstract rep for varmaps
         (check (list? subst))
         (check (list? facts))
         (new subst facts)))))

  (define empty-state
    (make-state empty-varmap '()))

  (define state=?
    (conjoin (on and-proc state?)
             (on equal? state-subst)
             (on equal? state-facts)))

  ;; State -> Constraint -> (Var -> Bool) -> (Listof Varmap)
  ;; For a given state and a constraint, what variable maps
  ;; currently hold in the system?
  (define (constraint-holds? state constraint metavar?)
    (check (constraint? constraint))
    (check (state? state))
    (cond
      [(debug? constraint)
       (puts state)
       (list (state-subst state))]
      [(scheme-check? constraint)
       (let-values ([(pred . objs)
                     (apply values (constraint-operands constraint))])
         (if (apply pred (walk* objs (state-subst state)))
             (list (state-subst state))
             '()))]
      [(assignment? constraint)
       (let*-values ([(lhs rhs) (apply values (constraint-operands constraint))]
                     [(vm1) (unify lhs rhs (state-subst state) metavar?)])
         (if vm1 (list vm1) '()))]
      [else
       (filter-map
        (lambda (other)
          (unify-constraint constraint other (state-subst state) metavar?))
        (state-facts state))]))

  ;; State -> (or Constraint (Listof Constraint)) -> Stream
  (define (constrain state cs)
    (check (state? state))
    (let* ([cs (if (constraint? cs) (list cs) cs)]
           [cs (filter
                (lambda (c) (null? (constraint-holds? state c (const #f))))
                cs)])
      (if (null? cs)
          (make-singleton state)
          (let*-values ([(assignments constraints) (partition assignment? cs)]
                        [(facts) (append constraints (state-facts state))]
                        [(subst) (varmap-extend-all
                                  (map
                                   (compose tuple->pair constraint-operands)
                                   assignments)
                                  (state-subst state))]
                        [(state1) (make-state subst facts)])
            (propagate-constraints state1)))))

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

  ;; TODO: make this into an operator that returns a goal
  ;; Rule -> Varmap -> List Consequence
  (define (instantiate-consequences rule vmap)
    (if (find failure? (rule-consequences rule))
        (list fail)
        (filter-map
         ;; TODO: This only considers postings
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
      (check (list? vmap))
      (check (list? prereqs))

      (if (null? prereqs)
          vmap
          (let* ([prereq (car prereqs)]
                 [ps (constraint-holds?
                      (make-state vmap (state-facts state))
                      (posting-constraint prereq)
                      (rule-var? rule))])
            (check (list? ps))
            (check (for-all list? ps))
            (find (lambda (as) (find-assignment as (cdr prereqs))) ps))))
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
    (check (state? state))
    (check (list? arguments))
    (check (for-all var? arguments))

    (values (walk* arguments (state-subst state))
            (state-facts state))))
