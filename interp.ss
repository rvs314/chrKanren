#!r6rs

(library (chrKanren interp)
  (export mature? mature age)
  (import (rnrs)
          (chrKanren utils) (chrKanren vars)
          (chrKanren goals) (chrKanren streams)
          (chrKanren relation) (chrKanren subst)
          (chrKanren state))

  (define (mature? strm)
    (and (or (empty? strm) (or (solution? strm)))))

  (define (mature strm)
    (if (mature? strm) strm (mature (step strm))))

  (define (age strm)
    (if (mature? strm) strm (step strm)))

  (define (start st gl)
    (cond
      [(failure? gl)     empty-stream]
      [(success? gl)     (make-solution st empty-stream)]
      [(disjunction? gl) (make-choice
                          (start st (disjunction-left gl))
                          (start st (disjunction-right gl)))]
      [(conjunction? gl) (make-bind (make-pause st (conjunction-left gl))
                                    (conjunction-right gl))]
      [(delay? gl)       (make-pause st gl)]
      [(call? gl)        (start st (apply (call-target gl) (call-arguments gl)))]
      [(projection? gl)  (let* ([vars  (projection-vars gl)]
                                [subst (state-subst st)]
                                [vars* (map (lambda (v) (walk* v subst)) vars)]
                                [goal  (apply (projection-cont gl) vars*)])
                           (start st goal))]
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
      [(bind? strm) (let ([s (age (bind-state strm))]
                          [g (bind-goal strm)])
                      (cond
                        [(empty? s) s]
                        [(solution? s)
                         (step (make-choice (make-pause (solution-first s) g)
                                            (make-bind (solution-rest s) g)))]
                        [else strm]))]
      [(pause? strm) (start (pause-state strm) (pause-goal strm))]
      [else          strm]))

  (define (take n strm)
    (cond
      [(or (zero? n) (empty? strm))
       '()]
      [(solution? strm)
       (cons (solution-first strm)
             (take (- n 1) (solution-rest strm)))]
      [else
       (take n (step strm))])))
