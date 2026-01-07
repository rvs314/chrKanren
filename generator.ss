#!r6rs

(library (chrKanren generator)
  (export random-choice weight random-case
          generator random-generator
          *default-random-size*
          value-generator varmap-generator state-generator
          goal-generator stream-generator
          random-var)
  (import (rnrs)
          (chrKanren check)
          (chrKanren utils)
          (chrKanren relation)
          (chrKanren goals)
          (chrKanren streams)
          (chrKanren vars)
          (chrKanren varmap)
          (chrKanren state)
          (srfi :39 parameters)
          (srfi :27 random-bits))

  (define-syntax weight (syntax-rules ()))

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
    (check (pair? xs))
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


  (define *default-random-size* (make-parameter 100))

  (define generator
    (case-lambda
      [(proc) (generator (*default-random-size*) proc)]
      [(mx proc) (case-lambda
                    [()   (proc (random-integer (max 1 mx)))]
                    [(mx) (proc (random-integer (max 1 mx)))])]))

  (define random-relation-counter 0)

  (define relation-generator
    (generator
     (lambda (k)
       (define the-body (goal-generator (- k 1)))
       (define new-relation
         (make-relation
          (string->symbol
           (string-append "random-relation-"
                          (number->string random-relation-counter)))
          (list)
          (lambda () the-body)))
       (set! random-relation-counter (+ 1 random-relation-counter))
       new-relation)))

  (define (make-random-generator gen1 gen+)
    (generator
     (lambda (k)
       (if (>= k 3)
           ((apply random-choice gen+) (- k 1))
           ((apply random-choice gen1))))))

  (define (split-evenly combine)
    (lambda (k+j)
      (set! k+j (max k+j 0))
      (let* ([k (random-integer (+ 1 k+j))]
             [j (- k+j k)])
        (combine (+ 1 k) (+ 1 j)))))

  (define-syntax-rule (random-generator subterms ...)
    (random-generator-aux (subterms ...) () ()))

  (define-syntax random-generator-aux
    (syntax-rules ()
      [(random-generator-aux () (gen1 ...) (gen+ ...))
       (make-random-generator
        (list gen1 ...)
        (list gen+ ...))]
      [(random-generator-aux ([() body ...] more ...)
                             (gen1 ...)
                             g2)
       (random-generator-aux (more ...)
                             ((lambda () body ...) gen1 ...)
                             g2)]
      [(random-generator-aux ([(x) body ...] more ...)
                             g1
                             (gen2 ...))
       (random-generator-aux (more ...)
                             g1
                             ((lambda (x) body ...) gen2 ...))]    
      [(random-generator-aux ([(x y) body ...] more ...)
                             g1
                             (gen2 ...))
       (random-generator-aux (more ...)
                             g1
                             ((split-evenly (lambda (x y) body ...))
                              gen2 ...))]))   

  (define goal-generator
    (random-generator
     [() succeed]
     [() fail]
     [(x) (make-call (relation-generator x) '())]
     [(l r) (disj (goal-generator l) (goal-generator r))]
     [(l r) (conj (goal-generator l) (goal-generator r))]
     ;; TODO: replace this clause with syntactic equality primitive
     ;; and upgrade generator to include constraint calls
     #;[(l r) (== (value-generator l) (value-generator r))]))

  (define stream-generator
    (random-generator
     [()    empty-stream]
     [(l r) (make-solution (state-generator l) (stream-generator r))]
     [(l r) (make-bind (stream-generator l) (goal-generator r))]
     [(l r) (make-choice (stream-generator l) (stream-generator r))]
     [(l r) (make-pause (state-generator l) (goal-generator r))]))

  (define var-names '(a b c d e f g h
                        i j k l m n o
                        p q r s t u v
                        w x y z
                        foo bar baz
                        qux quux quuux))

  (define *random-vars* (make-parameter '()))

  (define (random-var)
    (if (or (null? (*random-vars*)) (zero? (random-integer 5)))
        (let ([new (make-var (apply random-choice var-names))])
          (*random-vars* (cons new (*random-vars*)))
          new)
        (apply random-choice (*random-vars*))))

  (define varmap-generator
    (random-generator
     [()    empty-varmap]
     [(l r) (or (varmap-extend (random-var)
                               (value-generator l)
                               (varmap-generator r))
                (varmap-generator (+ l r)))]))

  (define value-generator
    (random-generator
     [()    '()]
     [()    (- (random-integer 100) 50)]
     [()    (random-var)]
     [(l r) (cons (value-generator l) (value-generator r))]))

  (define (state-generator . xs)
    (make-state (apply varmap-generator xs) '())))
