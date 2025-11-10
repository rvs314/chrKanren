#!r6rs

(library (chrKanren state)
  (export empty-state state state?
          constrain make-state query
          state=? state-facts state-subst)
  (import (rnrs)
          (only (srfi :1 lists) filter-map)
          (srfi :2 and-let*)
          (chrKanren utils)
          (racket trace)
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
         (if (apply pred (walk* objs (state-subst state)))
             (list (cons (state-subst state) constraint))
             '()))]
      [(assignment? constraint)
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

  ;; State -> (or Constraint (Listof Constraint)) -> Stream
  (define (constrain state cs)
    (check (state? state))
    (let* ([cs (if (constraint? cs) (list cs) cs)]
           [cs (filter
                (lambda (c)
                  (null? (query-constraint state c (const #f) '())))
                cs)])
      (let loop ([state state]))
      (if (null? cs)
          (make-singleton state)
          (let*-values ([(assignments constraints)
                         (partition assignment? cs)]
                        [(facts)
                         (append constraints (state-facts state))]
                        [(subst)
                         (varmap-extend-all
                          (map
                           (compose tuple->pair constraint-operands)
                           assignments)
                          (state-subst state))]
                        [(state1) (make-state subst facts)])
            (propagate-constraints state1)))))

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

  ;; Rule -> Varmap -> List Consequence
  (define (instantiate-consequences rule vmap)
    (if (find failure? (rule-consequences rule))
        (list fail)
        (filter-map
         ;; TODO: This only considers postings
         (lambda (consequence)
           (and (posting? consequence)
                (let* ([con (posting-constraint consequence)])
                  (make-constraint
                   (constraint-constructor con)
                   (constraint-reifier con)
                   (walk* (constraint-operands con) vmap (rule-var? rule))))))
         (rule-consequences rule))))

  ;; State -> List Constraint -> State
  (define (retract state witnesses)
    (check (state? state))
    (check ((listof constraint?) witnesses))
    (make-state
     (state-subst state)
     (filter (lambda (c) (not (memq c witnesses)))
             (state-facts state))))

  #;(-> State Rule (or #f (Pairof)))
  (define (apply-rule state rule)
    #;(-> Varmap
          (Listof Posting)
          (or #f (Pairof Varmap (Listof Constraint))))
    (define (find-assignment vmap prereqs seen)
      (check (varmap? vmap) "find-assignment")
      (check ((listof posting?) prereqs))

      (if (null? prereqs)
          (cons vmap '())
          (let* ([prereq (car prereqs)]
                 [potential-inst (query-constraint
                                  (make-state vmap (state-facts state))
                                  (posting-constraint prereq)
                                  (rule-var? rule)
                                  seen)])
            (exists (lambda (varmap.witness)
                      (check ((pairof varmap? constraint?)
                              varmap.witness))
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

  ;; State -> Stream
  (trace-define (propagate-constraints state)
    (check (state? state))
    (or (exists
         (lambda (rule)
           (check (rule? rule))
           (and-let* ([consequences.witnesses (apply-rule state rule)]
                      [consequences (car consequences.witnesses)]
                      [witnesses (cdr consequences.witnesses)])
             (if (find failure? consequences)
                 empty-stream
                 (constrain (retract state witnesses) consequences))))
         (*constraint-handling-rules*))
        (make-singleton state)))

  (define (contains? needle haystack)
    (or (equal? needle haystack)
        (and (pair? haystack)
             (or (contains? needle (car haystack))
                 (contains? needle (cdr haystack))))))

  (define (free-variables obj)
    (cond
      [(var? obj) (list obj)]
      [(pair? obj) (append (free-variables (car obj)) (free-variables (cdr obj)))]
      [else '() ]))

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

    (let* ([term      (walk* arguments (state-subst state))]
           [variables (free-variables term)]
           [facts     (filter (relevant? variables) (state-facts state))])
      (values term
              (map walk*-arguments facts)))))
