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
       (lambda (subst facts)
         (check (varmap? subst))
         (check (list? facts))
         (new subst facts)))))

  (define empty-state
    (make-state empty-varmap '()))

  (define state=?
    (conjoin (on and-proc state?)
             (on equal? state-subst)
             (on equal? state-facts)))

  ;; State -> Constraint -> (Var -> Bool)
  ;;   -> (Listof Constraint)
  ;;   -> (Listof (Cons Varmap Constraint))
  ;; For a given state and a constraint with metalogical variables satisfying
  ;; a given predicate which are not in the nogood list,
  ;; return: the set of "ground" varmaps which satisfy the constraint
  ;;         their witnesses in the database.
  (define (query-constraint state constraint metavar? nogood)
    (check (constraint? constraint))
    (check (state? state))
    (check (procedure? metavar?))
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

  ;; State -> (or Constraint (Listof Constraint)) -> (or #f State)
  (define (constrain state cs)
    (check (state? state))
    (check ((disjoin constraint? (listof constraint?)) cs))
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

  ;; Constraint -> Constraint -> Varmap -> Var? -> (or Varmap #f)
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

  ;; Rule -> Varmap -> Goal
  (define (instantiate-consequences rule vmap)
    (apply (rule-consequences rule)
           (walk* (rule-free-variables rule) vmap)))

  ;; State -> List Constraint -> State
  (define (retract state witnesses)
    (check (state? state))
    (check ((listof constraint?) witnesses))
    (make-state
     (state-subst state)
     (filter (lambda (c) (not (memq c witnesses)))
             (state-facts state))))

  ;; State -> Rule -> (or #f (Pairof Goal (Listof Constraint)))
  (define (apply-rule state rule)
    ;; Varmap -> Listof Posting -> Listof Constraint ->
    ;; (or #f (Pairof Varmap (Listof Constraint)))
    (define (find-assignment vmap prereqs seen)
      (check (varmap? vmap) "find-assignment")
      (check ((listof posting?) prereqs))
      (check ((listof constraint?) seen))

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
    (check (state? state))
    (check (rule? rule))

    (let ([as (find-assignment
               (state-subst state)
               (rule-prereqs rule)
               '())])
      (and as (cons (instantiate-consequences rule (car as))
                    (cdr as)))))

  (define (contains? needle haystack)
    (find-subtree (lambda (subtree) (equal? needle subtree)) haystack))

  ;; State -> Listof Var -> (Values (Listof Term) (Listof Constraint))
  (define (query state arguments)
    (define (relevant? vars)
      (lambda (fact)
        (exists (lambda (arg) (exists (lambda (op) (contains? arg op))
                                      (constraint-operands fact)))
                vars)))
    (define (walk*-arguments con)
      (check (constraint? con))
      (make-constraint (constraint-constructor con)
                       (constraint-reifier con)
                       (map (lambda (a) (walk* a (state-subst state)))
                            (constraint-operands con))))
    (check (state? state))
    (check ((listof var?) arguments))

    (values (walk* arguments (state-subst state))
            (map walk*-arguments (state-facts state)))))
