#!r6rs

(library (chrKanren generator)
  (export random-choice *random-size* random-proc
          goal-generator value-generator relation-generator
          stream-generator state-generator)
  (import (rnrs)
          (chrKanren utils)
          (chrKanren relation)
          (chrKanren goals)
          (chrKanren streams)
          (chrKanren check)
          (chrKanren vars)
          (chrKanren varmap)
          (chrKanren state)
          (only (srfi :1 lists)
                filter-map make-list list-tabulate append-map)
          (srfi :26 cut)
          (srfi :39 parameters)
          (srfi :27 random-bits))

  (define *random-size* (make-parameter 100))

  (define-check (random-choice [xs list?])
    any?
    (list-ref xs (random-integer (length xs))))

  (define-check (random-proc [option1 procedure?]
                             :rest
                             [options (listof procedure?)])
    procedure?
    (lambda xs
      (apply (random-choice (cons option1 options)) xs)))

  (define-check (induction-generator [base procedure?]
                                     [ind procedure?])
    procedure?
    (lambda-check ([sz natural?])
      any?
      (if (>= (or (and (positive? sz) (/ 1 sz)) 1.0) (random-real))
          (base (- sz 1))
          (ind  (- sz 1)))))

  (define-check (list-generator [el procedure?])
    procedure?
    (eta
     (induction-generator
      (const '())
      (compose cons (broadcast el (list-generator el))))))

  (define symbols '(a b c d e f g h
                      i j k l m n o
                      p q r s t u v
                      w x y z
                      foo bar baz
                      qux quux quuux))

  (define *vars-in-scope* (make-parameter '()))

  (define-check (var-generator _)
    (disjoin* not var?)
    (and (pair? (*vars-in-scope*))
         (random-choice (*vars-in-scope*))))

  (define state-generator (const empty-state))

  (define-check (random-var)
    var?
    (define name (random-choice symbols))
    (make-var name))

  (define value-generator
    (eta
     (induction-generator
      (random-proc (const '())
                   (const* (- (random-integer 100) 50))
                   (const* (random-choice symbols))
                   var-generator)
      (compose cons (broadcast value-generator value-generator)))))

  (define *random-relations* (make-parameter '()))

  (define (new-relation-generator k)
    (define name (symbol "random-relation-" (length (*random-relations*))))
    (define arity (random-integer 3))
    (define arg-vars (list-tabulate arity (const* (random-var))))
    (define body (parameterize ([*vars-in-scope* (append arg-vars (*vars-in-scope*))])
                   (goal-generator k)))
    (define rel
      (make-relation
       name
       (map var-name arg-vars)
       (case arity
         [(0) (lambda () body)]
         [(1) (let ([v (car arg-vars)])
                (lambda (a) (conj (=== a v) body)))]
         [(2) (let ([v1 (car arg-vars)] [v2 (cadr arg-vars)])
                (lambda (a b) (conj (=== a v1) (=== b v2) body)))])))
    (*random-relations* (cons rel (*random-relations*)))
    rel)

  (define-check (relation-generator [k natural?])
    relation?
    (if (null? (*random-relations*))
        (new-relation-generator k)
        (induction-generator
         (const* (random-choice (*random-relations*)))
         new-relation-generator)))

  (define goal-generator
    (eta (induction-generator
          (random-proc (const succeed)
                       (const fail))
          (random-proc
           (compose (random-proc disj conj)
                    (broadcast goal-generator goal-generator))
           (compose === (broadcast value-generator value-generator))
           (lambda-check ([sz natural?])
             goal?
             (fresh (p)
               (parameterize ([*vars-in-scope* (cons p (*vars-in-scope*))])
                 (goal-generator sz))))
           (lambda-check ([sz natural?])
             call?
             (define rel  (relation-generator sz))
             (define args
               (list-tabulate (length (relation-args rel))
                              (const* (value-generator sz))))
             (make-call rel args))))))

  (define stream-generator
    (eta (induction-generator
          (const empty-stream)
          (random-proc
           (compose make-solution    (broadcast state-generator stream-generator))
           (compose make-bind        (broadcast stream-generator goal-generator))
           (compose make-choice      (broadcast stream-generator stream-generator))
           (compose make-paused-step (broadcast state-generator goal-generator)))))))
