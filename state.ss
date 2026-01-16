#!r6rs

(library (chrKanren state)
  (export empty-state state state?
          constrain make-state query
          apply-rule retract
          state=? state-facts state-subst)
  (import (rnrs)
          (only (srfi :1 lists) filter-map)
          (srfi :2 and-let*)
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
       (define-check (make-state [subst varmap?] [facts list?])
         state?
         (new subst facts))
       make-state)))

  (define empty-state
    (make-state empty-varmap '()))

  (define state=?
    (conjoin (on and-proc state?)
             (on equal? state-subst)
             (on equal? state-facts)))

  ;; For a given state and a constraint with metalogical variables satisfying
  ;; a given predicate which are not in the nogood list,
  ;; return: the set of "ground" varmaps which satisfy the constraint
  ;;         their witnesses in the database.
  (define-check (query-constraint [state state?]
                                  [constraint constraint?]
                                  [metavar? procedure?]
                                  [nogood (listof constraint?)])
    (listof (pairof varmap? constraint?))
    (cond
      [(scheme-check? constraint)
       (let-values ([(pred . objs)
                     (apply values (constraint-operands constraint))])
         (if (apply (walk* pred (state-subst state))
                    (walk* objs (state-subst state)))
             (list (cons (state-subst state) constraint))
             '()))]
      [(same-check? constraint)
       (let*-values ([(lhs rhs) (apply values (constraint-operands constraint))]
                     [(vm1) (unify lhs rhs (state-subst state) metavar?)])
         (if vm1 (list (cons vm1 constraint)) '()))]
      [else
       (filter-map
        (lambda (other)
          (and-let* ([_ (not (member other nogood))]
                     [vm (unify-constraint constraint other (state-subst state) metavar?)])
            (cons vm other)))
        (state-facts state))]))

  (define-check (constrain [state state?] [cs (disjoin constraint? (listof constraint?))])
    (disjoin not state?)
    (let* ([cs (if (constraint? cs) (list cs) cs)]
           [cs (filter
                (lambda (c)
                  (null? (query-constraint state c (const #f) '())))
                cs)])
      (if (null? cs)
          state
          (let*-values ([(same-checks constraints)
                         (partition same-check? cs)]
                        [(facts)
                         (append (state-facts state) constraints)]
                        [(subst)
                         (unify*
                          (map
                           (compose tuple->pair constraint-operands)
                           same-checks)
                          (state-subst state))]
                        [(state1) (and subst (make-state subst facts))])
            state1))))

  (define-check (unify-constraint [lcon rcon constraint?]
                                  [vmap varmap?]
                                  [var? procedure?])
    (disjoin not varmap?)
    (and (eq? (constraint-constructor lcon)
              (constraint-constructor rcon))
         (unify (constraint-operands lcon)
                (constraint-operands rcon)
                vmap
                var?)))

  (define-check (rule-var? [rule rule?])
    procedure?
    (lambda (vr)
      (memq vr (rule-free-variables rule))))

  (define-check (instantiate-consequences [rule rule?] [vmap varmap?])
    goal?
    (apply (rule-consequences rule)
           (walk* (rule-free-variables rule) vmap)))

  (define-check (retract [state state?] [witnesses (listof constraint?)])
    state?
    (make-state
     (state-subst state)
     (filter (lambda (c) (not (memq c witnesses)))
             (state-facts state))))

  (define-check (apply-rule [state state?] [rule rule?])
    (disjoin not (pairof goal? (listof constraint?)))
    (define-check (find-assignment [vmap varmap?]
                                   [prereqs (listof posting?)]
                                   [seen (listof constraint?)])
      (disjoin not (pairof varmap? (listof constraint?)))

      (if (null? prereqs)
          (cons vmap '())
          (let* ([prereq (car prereqs)]
                 [potential-inst (query-constraint
                                  (make-state vmap (state-facts state))
                                  (posting-constraint prereq)
                                  (rule-var? rule)
                                  seen)])
            (exists (lambda (varmap.witness)
                      (check ((pairof varmap? constraint?) varmap.witness))
                      (and-let* ([witness (cdr varmap.witness)]
                                 [varmap (car varmap.witness)]
                                 [varmap^.witnesses
                                  (find-assignment varmap
                                                   (cdr prereqs)
                                                   (cons witness seen))])
                        (cons (car varmap^.witnesses)
                              (cons witness (cdr varmap^.witnesses)))))
                    potential-inst))))
    (and-let* ([as (find-assignment
                    (state-subst state)
                    (rule-prereqs rule)
                    '())]
               [consq (instantiate-consequences rule (car as))])
      (cons consq (cdr as))))

  (define-check (query [state state?] [args (listof var?)])
    (arguments list? (listof constraint?))
    (define-check (walk*-arguments [con constraint?])
      constraint?
      (make-constraint (constraint-constructor con)
                       (constraint-reifier con)
                       (map (lambda (a) (walk* a (state-subst state)))
                            (constraint-operands con))))
    (values (walk* args (state-subst state))
            (map walk*-arguments (state-facts state)))))
