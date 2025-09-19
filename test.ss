#!r6rs

(library (chrKanren test)
  (export define-random-test *random-test-count*
          random-case weight
          random-relation random-stream random-value
          random-state random-goal random-var
          test-runner-install-random-hooks!
          *finite-step-count* mature-finite take-finite)
  (import (rnrs)
          (chrKanren utils)
          (chrKanren relation)
          (chrKanren goals)
          (chrKanren vars)
          (chrKanren interp)
          (chrKanren state)
          (chrKanren subst)
          (racket trace)
          (chrKanren state)
          (chrKanren streams)
          (only (srfi :1 lists) list-tabulate drop-right concatenate pair-fold)
          (srfi :64 testing)
          (srfi :39 parameters)
          (srfi :27 random-bits))

  (define *random-test-count*
    (make-parameter 30))

  (define-syntax weight
    (syntax-rules ()))

  (define-syntax random-case-aux
    (syntax-rules (weight)
      [(random-case-aux
        var
        prior-weight
        [[condition body body* ...] ...]
        [])
       (let ((var (random-integer prior-weight)))
         (cond
           [condition body body* ...]
           ...))]
      [(random-case-aux
        var
        prior-weight
        [prior-case ...]
        [[(weight w) body ...] rst ...])
       (random-case-aux
        var
        (+ prior-weight w)
        [prior-case ... [(< var (+ prior-weight w)) body ...]]
        [rst ...])]
      [(random-case-aux
        var
        prior-weight
        [prior-case ...]
        [[body ...] rst ...])
       (random-case-aux
        var
        prior-weight
        [prior-case ...]
        [[(weight 1) body ...] rst ...])]))

  (define (random-choice . xs)
    (list-ref xs (random-integer (length xs))))

  (define-syntax random-case
    (syntax-rules ()
      [(random-case
        [b b* ...] [c c* ...] ...)
       (random-case-aux
        random-var
        0
        []
        [[b b* ...] [c c* ...] ...])]))

  (define *current-batch-index* (make-parameter #f))
  (define *current-random-variables* (make-parameter '()))

  (define (test-runner-install-random-hooks! base)
    (define super (test-runner-on-test-end base))
    (test-runner-on-test-end! base
      (lambda (rnr)
        (test-result-set! rnr 'batch-size (*random-test-count*))
        (when (*current-batch-index*)
          (test-result-set! rnr 'batch-index (*current-batch-index*)))
        (for-each
         (lambda (pr)
           (define the-obj (cons 'the 'obj))
           (when (eq? the-obj (test-result-ref rnr (car pr) the-obj))
             (test-result-set! rnr (car pr) (cdr pr))))
         (*current-random-variables*))
        (super rnr)))
    base)

  (define-syntax define-random-test
    (syntax-rules ()
      [(define-random-test (name (var val) ...)
         body ...)
       (test-group (symbol->string (quote name))
         (let [(c (*random-test-count*))]
           (do ((i 0 (+ i 1))) ((= i c))
             (let ((var val) ...)
               (parameterize ([*current-batch-index* i]
                              [*current-random-variables* `((var . ,val) ...)])
                 body ...)))))]))

  (define random-relation-counter 0)

  (define (random-relation)
    (define the-body (random-goal))
    (define new-relation
      (make-relation
       (string->symbol
        (string-append "random-relation-" (number->string random-relation-counter)))
       (list)
       (lambda () the-body)))
    (set! random-relation-counter (+ 1 random-relation-counter))
    new-relation)

  (define (random-goal)
    (random-case
     [(weight 2) succeed]
     [(weight 2) fail]
     [(disj (random-goal) (random-goal))]
     [(conj (random-goal) (random-goal))]
     [(make-call (random-relation) '())]
     [(== (random-value) (random-value))]
     [(let ([g (random-goal)])
        (Zzz g))]))

  (define (random-stream)
    (random-case
     [empty-stream]
     [(make-solution (random-state) (random-stream))]
     [(make-bind (random-stream) (random-goal))]
     [(make-choice (random-stream) (random-stream))]
     [(make-pause (random-state) (random-goal))]))

  (define sample-vars (map make-var '(a b c d e f g h
                                        i j k l m n o
                                        p q r s t u v
                                        w x y z)))

  (define (random-var)
    (apply random-choice sample-vars))

  (define (random-subst)
    (random-case
     [empty-subst]
     [(or (extend (random-var) (random-value) (random-subst))
          (random-subst))]))

  (define (random-value)
    (random-case
     ['()]
     [(- (random-integer 60) 30)]
     [(random-var)]
     [(cons (random-value) (random-value))]))

  (define (random-state)
    (make-state (random-subst)))

  (define *finite-step-count* (make-parameter 300))

  (define (mature-finite strm)
    (let loop ([strm strm]
               [idx (*finite-step-count*)])
      (cond
        [(mature? strm) strm]
        [(zero? idx) (error 'mature-finite "Stream is immature")]
        [else (loop (step strm) (- idx 1))])))

  (define (take-finite strm)
    (let loop ([strm strm]
               [idx (*finite-step-count*)]
               [acc '()])
      (let ([strm* (mature-finite strm)])
        (cond
          [(empty? strm*)    (reverse acc)]
          [(zero? idx)       (error 'rite-of-passage "Stream is infinite")]
          [(solution? strm*) (loop (solution-rest strm*)
                                   (- idx 1)
                                   (cons (solution-first strm*) acc))]))))

  (define test-≈
    (case-lambda
      [(name left right)
       (cond
         [(and (stream? left) (stream? right))
          (and (goal? left) (goal? right))])]
      [(left right)
       (test-≈ #f left right)])))
