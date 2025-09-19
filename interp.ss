#!r6rs

(library (chrKanren interp)
  (export mature age take step start)

  (import (rnrs)
          (chrKanren utils) (chrKanren vars)
          (chrKanren goals) (chrKanren streams)
          (chrKanren relation) (chrKanren subst)
          (chrKanren state) (chrKanren unify))

  (define (mature strm)
    (if (mature? strm) strm (mature (step strm))))

  (define (age strm)
    (if (mature? strm) strm (step strm)))

  (define (start st gl)
    (cond
      [(failure? gl)     empty-stream]
      [(success? gl)     (make-singleton st)]
      [(disjunction? gl) (make-choice
                          (start st (disjunction-left gl))
                          (start st (disjunction-right gl)))]
      [(conjunction? gl) (make-bind (make-pause st (conjunction-left gl))
                                    (conjunction-right gl))]
      [(delay? gl)       (make-pause st ((delay-cont gl)))]
      [(call? gl)        (start st (apply call-relation
                                          (call-target gl)
                                          (call-arguments gl)))]
      [(unification? gl) (let* ([lhs (unification-lhs gl)]
                                [rhs (unification-rhs gl)])
                           (unify lhs rhs st))]
      [else (error 'start "Can't start goal" st gl)]))

  (define (step strm)
    (cond
      [(choice? strm) (let ([s1 (age (choice-left strm))]
                            [s2 (choice-right strm)])
                        (cond
                          [(empty? s1) s2]
                          [(solution? s1)
                           (make-solution (solution-first s1)
                                          (make-choice s2 (solution-rest s1)))]
                          [else (make-choice s2 s1)]))]
      [(bind? strm) (let ([s (age (bind-stream strm))]
                          [g (bind-goal strm)])
                      (cond
                        [(empty? s) s]
                        [(solution? s)
                         (make-choice (make-pause (solution-first s) g)
                                      (make-bind (solution-rest s) g))]
                        [else (make-bind s g)]))]
      [(pause? strm) (start (pause-state strm) (pause-goal strm))]
      [else          strm]))

  (define take
    (case-lambda
      [(strm)
       (take +inf.0 strm)]
      [(n strm)
       (cond
         [(or (zero? n) (empty? strm))
          '()]
         [(solution? strm)
          (cons (solution-first strm)
                (take (- n 1) (solution-rest strm)))]
         [else
          (take n (step strm))])])))
