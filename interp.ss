#!r6rs

(library (chrKanren interp)
  (export *maturation-limit* mature age
          unify-constraint apply-rule constrain
          take+drop take drop
          step start
          make-seen-before?
          propagate-constraints apply-rule query-constraint)

  (import (rnrs)
          (chrKanren check)
          (chrKanren rule)
          (chrKanren utils)
          (chrKanren vars)
          (chrKanren varmap)
          (chrKanren goals)
          (chrKanren streams)
          (chrKanren relation)
          (chrKanren state)
          (chrKanren unify)
          (chrKanren goals)
          (only (srfi :1 lists) filter-map)
          (srfi :2 and-let*)
          (srfi :26 cut)
          (srfi :39 parameters)
          (except (prefix (srfi :41 streams) s:)
                  s:list->stream s:stream->list))

  ;; Because racket defines R6RS "lists" as their mutable-lists,
  ;; any code they write that uses list primitives needs to be copied.
  (define (s:list->stream objs)
    (define list->stream
      (s:stream-lambda
       (objs)
       (if (null? objs)
           s:stream-null
           (s:stream-cons (car objs) (list->stream (cdr objs))))))
    (if (not (list? objs))
        (error 'list->stream "non-list argument")
        (list->stream objs)))

  (define (s:stream->list strm)
    (if (s:stream-pair? strm)
        (cons (s:stream-car strm)
              (s:stream->list (s:stream-cdr strm)))
        '()))

  (define-check (unify-constraint [lcon rcon constraint?]
                                  [subst varmap?]
                                  [var? procedure?])
    (disjoin* not varmap?)
    (and (eq? (constraint-constructor lcon)
              (constraint-constructor rcon))
         (unify (constraint-operands lcon)
                (constraint-operands rcon)
                subst
                var?)))

  #|
  Given a state and a constraint, return a stream of
  potential (subst/witness pairs)
  |#
  (define-check (query-constraint [state state?]
                                  [constraint constraint?]
                                  [metavar? procedure?])
    s:stream?
    (cond
      [(scheme-check? constraint)
       (let-values ([(pred . objs)
                     (apply values (constraint-operands constraint))])
         (if (apply (walk pred (state-subst state))
                    (walk* objs (state-subst state)))
             (s:stream (cons (state-subst state) #f))
             (s:stream)))]
      [(same-check? constraint)
       (let*-values ([(lhs rhs)
                      (apply values (constraint-operands constraint))]
                     [(vm1)
                      (unify lhs rhs (state-subst state) metavar?)])
         (if vm1
             (s:stream (cons vm1 #f))
             (s:stream)))]
      [else
       (s:stream-of
        (cons vm fact)
        (fact in (s:list->stream (state-facts state)))
        (vm is (unify-constraint constraint
                                 fact
                                 (state-subst state)
                                 metavar?))
        (values vm))]))

  (define-check (constrain [state state?]
                           [cs constraint?])
    (disjoin* not state?)
    (let*-values ([(cs) (list cs)]
                  [(same-checks constraints)
                   (partition same-check? cs)]
                  [(facts)
                   (append (state-facts state) constraints)]
                  [(hist) (state-hist state)]
                  [(subst)
                   (unify*
                    (map
                     (compose tuple->pair constraint-operands)
                     same-checks)
                    (state-subst state))]
                  [(state1)
                   (and subst
                        (make-state subst facts hist))])
      state1))

  (define-check (make-seen-before? [rule rule?] [state state?])
    procedure?
    (define-check (seen-before? [witnesses (listof constraint?)])
      (disjoin* not history-entry?)
      (state-seen-application? state rule witnesses))
    seen-before?)

  (define-check (apply-rule [rule rule?] [state state?])
    (disjoin* not propagating?)
    (define rule-variable? (cut rule-free? rule <>))
    ;; Solves a list of prerequisites for a given substitution
    ;; Returns a stream of possible solutions, which are
    ;; pairs of substitutions and witness lists
    ;; (: solve-prereqs ((listof witness?) varmap?
    ;;                   -> (streamof (pairof varmap?
    ;;                                        (listof witness?)))))
    (define-check (solve-prereqs [prereqs (listof witness?)]
                                 [subst varmap?])
      s:stream?
      (disjoin (arguments not not)
               (arguments varmap? (listof witness?)))
      (if (null? prereqs)
          (s:stream (cons subst '()))
          (s:stream-of (cons subst^^
                             (if witness^
                                 (cons witness^ witnesses)
                                 witnesses))
                       (prereq is (car prereqs))
                       (prereq-constraint is (witness->constraint
                                              prereq))
                       (matches is (query-constraint
                                    (state-with-subst subst state)
                                    prereq-constraint
                                    rule-variable?))
                       (match in matches)
                       (subst^ is (car match))
                       (constraint^ is (cdr match))
                       (witness^ is
                                 (and constraint^
                                      (make-witness
                                       (witness-kept? prereq)
                                       (post constraint^))))
                       (solution in (solve-prereqs (cdr prereqs)
                                                   subst^))
                       (subst^^ is (car solution))
                       (witnesses is (cdr solution)))))
    (define witness->constraint
      (compose posting-constraint witness-constraint))
    (and-let* ([solutions
                (s:stream-filter
                 (compose
                  (conjoin all-distinct?
                           (negate (make-seen-before? rule state)))
                  (cut map witness->constraint <>)
                  cdr)
                 (solve-prereqs (rule-prereqs rule)
                                (state-subst state)))]
               [_ (s:stream-pair? solutions)]
               [sol (s:stream-car solutions)]
               [subst (car sol)]
               [witnesses (cdr sol)])
      (make-propagating
       (make-bind
        (make-singleton
         (state-record-application state rule witnesses))
        (let* ([args  (walk* (rule-free-variables rule) subst)]
               [args0 (copy-term args rule-variable?)])
          (apply (rule-consequences rule) args0))))))

  (define-check (propagate-constraints [state state?])
    (disjoin* propagating? solution?)
    (or (exists (cut apply-rule <> state)
                (*constraint-handling-rules*))
        (make-solution state empty-stream)))

  (define *maturation-limit* (make-parameter +inf.0))

  (define-check (mature [strm stream?])
    stream?
    (let loop ([strm strm] [count (*maturation-limit*)])
      (cond
        [(mature? strm) strm]
        [(zero? count) (error 'mature "Stream is infinite" strm)]
        [else (loop (step strm) (- count 1))])))

  (define-check (age [strm stream?])
    stream?
    (if (mature? strm)
        strm
        (step strm)))

  (define-check (start [st state?] [gl goal?])
    stream?
    (cond
      [(failure? gl)     empty-stream]
      [(success? gl)     (make-solution st empty-stream)]
      [(disjunction? gl)
       (step (make-choice (make-paused-step st (disjunction-left gl))
                          (make-paused-step st (disjunction-right gl))))]
      [(conjunction? gl)
       (step (make-bind (make-paused-step st (conjunction-left gl))
                        (conjunction-right gl)))]
      [(delay? gl)
       (make-paused-step st ((delay-cont gl)))]
      [(call? gl)
       (start st (apply call-relation
                        (call-target gl)
                        (call-arguments gl)))]
      [(posting? gl)
       (step (let ([s1 (constrain st (posting-constraint gl))])
                (if s1
                    (make-paused-propagation s1)
                    empty-stream)))]
      [else
       (assertion-violation 'start
                            "Not sure how to start goal"
                            st
                            gl)]))

  (define-check (step [strm stream?])
    stream?
    (cond
      [(choice? strm)
       (let ([s1 (age (choice-left strm))]
             [s2 (choice-right strm)])
         (cond
           [(empty? s1) s2]
           [(solution? s1)
            (make-solution (solution-first s1)
                           (make-choice s2 (solution-rest s1)))]
           [else (make-choice s2 s1)]))]
      [(propagating? strm)
       (let ([s (age (propagating-stream strm))])
         (cond
           [(empty? s) s]
           [(solution? s)
            (step (make-choice (make-paused-propagation (solution-first s))
                               (make-propagating (solution-rest s))))]
           [else (make-propagating s)]))]
      [(bind? strm)
       (let ([s (age (bind-stream strm))]
             [g (bind-goal strm)])
         (cond
           [(empty? s) s]
           [(solution? s)
            (step (make-choice (make-paused-step (solution-first s) g)
                               (make-bind (solution-rest s) g)))]
           [else (make-bind s g)]))]
      [(paused-step? strm)
       (start (paused-step-state strm) (paused-step-goal strm))]
      [(paused-propagation? strm)
       (propagate-constraints (paused-propagation-state strm))]
      [else          strm]))

  (define take+drop
    (case-lambda
      [(strm) (take+drop +inf.0 strm)]
      [(n strm)
       (if (zero? n)
           (values '() strm)
           (let ([strm (mature strm)])
             (cond
               [(empty? strm)
                (values '() strm)]
               [(solution? strm)
                (let-values ([(f r) (take+drop (- n 1) (solution-rest strm))])
                  (values (cons (solution-first strm) f)
                          r))]
               [else
                (error 'take+drop "Mature did not return a mature stream" strm)])))]))

  (define (take . ks)
    (let-values ([(f r) (apply take+drop ks)])
      f))

  (define (drop . ks)
    (let-values ([(f r) (apply take+drop ks)])
      r)))
