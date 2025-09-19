#!r6rs

(library (chrKanren test)
  (export with-random-test *random-test-count*
          random-case weight)
  (import (rnrs)
          (chrKanren utils)
          (chrKanren relation)
          (chrKanren goals)
          (chrKanren vars)
          (chrKanren subst)
          (chrKanren state)
          (chrKanren streams)
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


  (define-syntax random-case
    (syntax-rules ()
      [(random-case
        [b b* ...] [c c* ...] ...)
       (random-case-aux
        random-var
        0
        []
        [[b b* ...] [c c* ...] ...])]))

  (define-syntax with-random-test
    (syntax-rules ()
      [(with-random-test (name cnt) body ...)
       (let ((c cnt))
         (do ((i 0 (+ i 1))) ((= i c))
           (test-group
               (string-append name
                              " ("
                              (number->string i)
                              "/"
                              (number->string c)
                              ")")
             body ...)))]
      [(with-random-test name body ...)
       (with-random-test (name (*random-test-count*)) body ...)]))

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

  (define *vars* (make-parameter #f))

  (define (random-var)
    (list-ref (*vars*) (random-integer (length (*vars*)))))

  (define (random-value)
    (random-case
     ['()]
     [(random-integer 30)]
     [(random-var)]
     [(cons (random-value) (random-value))]))

  (define (random-state)
    (define var-names '(a b c d e f g h i j))

    (define (random-assoc)
      (random-case
       [empty-subst]
       [(weight 3) (or (extend (random-var) (random-value) (random-assoc))
                       (random-assoc))]))

    (parameterize ([*vars* (map make-var var-names)])
      (make-state (random-assoc))))

  (define (random-stream)
    (random-case
     [empty-stream]
     [(make-solution (random-state) (random-stream))]
     [(make-bind   (random-stream) (random-goal))]
     [(make-choice (random-stream) (random-stream))])))
