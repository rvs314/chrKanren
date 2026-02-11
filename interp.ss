#!r6rs

(library (chrKanren interp)
  (export *maturation-limit* mature age take+drop take drop step start)

  (import (rnrs)
          (chrKanren check)
          (chrKanren rule)
          (chrKanren utils)
          (chrKanren vars)
          (chrKanren goals)
          (chrKanren streams)
          (chrKanren relation)
          (chrKanren state)
          (srfi :2 and-let*)
          (srfi :39 parameters))

  (define-check (propagate-constraints [state state?])
    stream?
    (or (exists
         (lambda (rule)
           (check (rule? rule))
           (and-let* ([goal.witnesses (apply-rule state rule)]
                      [goal (car goal.witnesses)]
                      [witnesses (cdr goal.witnesses)])
             (make-propagating
              (start (retract state witnesses) goal))))
         (*constraint-handling-rules*))
        (make-solution state empty-stream)))

  (define *maturation-limit* (make-parameter +inf.0))

  (define (mature strm)
    (check (stream? strm) "Cannot mature a non-stream")
    (let loop ([strm strm] [count (*maturation-limit*)])
      (cond
        [(mature? strm) strm]
        [(zero? count) (error 'mature "Stream is infinite" strm)]
        [else (loop (step strm) (- count 1))])))

  (define (age strm)
    (if (mature? strm) strm (step strm)))

  ;; State -> Goal -> Stream
  (define (start st gl)
    (cond
      [(failure? gl)     empty-stream]
      [(success? gl)     (make-solution st empty-stream)]
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
       (let ([s1 (constrain st (posting-constraint gl))])
         (if s1
             (propagate-constraints s1)
             empty-stream))]
      [else
       (check #f "Not sure how to start goal" st gl)]))

  ;; Stream -> Stream
  (define (step strm)
    (check (stream? strm))
    (cond
      [(choice? strm)
       (let ([s1 (age (choice-left strm))]
             [s2 (choice-right strm)])
         (cond
           [(empty? s1) s2]
           [(solution? s1)
            (make-solution (solution-first s1)
                           (make-choice (solution-rest s1) s2))]
           [else (make-choice s2 s1)]))]
      [(propagating? strm)
       (let ([s (age (propagating-stream strm))])
         (cond
           [(empty? s) s]
           [(and (solution? s) (empty? (solution-rest s)))
            (propagate-constraints (solution-first s))]
           [(solution? s)
            (make-choice (propagate-constraints (solution-first s))
                         (make-propagating (solution-rest s)))]
           [else (make-propagating s)]))]
      [(bind? strm)
       (let ([s (age (bind-stream strm))]
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
      [(strm) (take+drop +inf.0 strm)]
      [(n strm)
       (let ([strm (mature strm)])
         (cond
           [(or (zero? n) (empty? strm))
            (values '() strm)]
           [(solution? strm)
            (let-values ([(f r) (take+drop (- n 1) (solution-rest strm))])
              (values (cons (solution-first strm) f)
                      r))]
           [else
            (error 'take+drop "Mature did not return a mature stream" strm)]))]))

  (define (take . ks)
    (let-values ([(f r) (apply take+drop ks)])
      f))

  (define (drop . ks)
    (let-values ([(f r) (apply take+drop ks)])
      r)))
