#!r6rs

(library (chrKanren interp)
  (export mature age take+drop take drop step start)

  (import (rnrs)
          (chrKanren check)
          (chrKanren utils) (chrKanren vars)
          (chrKanren goals) (chrKanren streams)
          (chrKanren relation)
          (chrKanren state))

  (define mature
    (case-lambda
      [(strm)
       (mature strm +inf.0)]
      [(strm k)
       (check (stream? strm) "Cannot mature a non-stream")
       (if (or (zero? k) (mature? strm))
           strm
           (mature (step strm) (- k 1)))]))

  (define (age strm)
    (if (mature? strm) strm (step strm)))

  ;; State -> Goal -> Stream
  (define (start st gl)
    (cond
      [(failure? gl)     empty-stream]
      [(success? gl)     (make-singleton st)]
      [(disjunction? gl)
       (step (make-choice (make-pause st (disjunction-left gl))
                          (make-pause st (disjunction-right gl))))]
      [(conjunction? gl)
       (step (make-bind (make-pause st (conjunction-left gl))
                        (conjunction-right gl)))]
      [(delay? gl)
       (make-pause st ((delay-cont gl)))]
      [(call? gl)
       (start st (apply call-relation
                        (call-target gl)
                        (call-arguments gl)))]
      [(posting? gl)
       (constrain st (posting-constraint gl))]
      [else
       (check #f "Not sure how to start goal" st gl)]))

  ;; Stream -> Stream
  (define (step strm)
    (check (stream? strm))
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
                         (step (make-choice (make-pause (solution-first s) g)
                                            (make-bind (solution-rest s) g)))]
                        [else (make-bind s g)]))]
      [(pause? strm) (start (pause-state strm) (pause-goal strm))]
      [else          strm]))

  (define take+drop
    (case-lambda
      [(strm)
       (take+drop +inf.0 strm)]
      [(n strm)
       (cond
         [(or (zero? n) (empty? strm))
          (values '() strm)]
         [(solution? strm)
          (let-values ([(f r) (take+drop (- n 1) (solution-rest strm))])
            (values (cons (solution-first strm) f)
                    r))]
         [else
          (take+drop n (step strm))])]))

  (define (take . ks)
    (let-values ([(f r) (apply take+drop ks)])
      f))

  (define (drop . ks)
    (let-values ([(f r) (apply take+drop ks)])
      r)))
