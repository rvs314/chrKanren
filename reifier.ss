#!r6rs

(library (chrKanren reifier)
  (export reify-query reify-constraint
          *variable-namer*
          make-helpful-variable-namer
          make-simple-variable-namer)
  (import (rnrs)
          (rnrs mutable-pairs)
          (srfi :39 parameters)
          (only (srfi :1 lists) list-index delete-duplicates filter-map)
          (chrKanren vars)
          (chrKanren compare)
          (chrKanren goals)
          (chrKanren check)
          (chrKanren unify)
          (chrKanren varmap)
          (chrKanren vars)
          (chrKanren utils)
          (chrKanren state))

  #|
  Reification with cyclic structures becomes very difficult
  very quickly, namely because we don't want to reify intermediate
  variables that aren't useful. Consider a few difficult cases:

  (run* (q) (=== q q))
  ↪ _.0

  (run* (q) (=== q (cons 1 q)))
  ↪ #0=(1 . #0#)

  (run* (q)
    (fresh (r)
      (=== q r)
      (=== r (cons 1 q))))
  ↪ #0=(1 . #0#)

  The last examples proves that some variables can be
  removed entirely, but others can't. Only variables which point
  to a compound structure should be considered
  for recursion checks.
  |#

  (define *variable-namer*
    (make-parameter
     (lambda ()
       (define name-counter 0)
       (lambda (_vr)
         (begin0 (symbol "_." name-counter)
           (set! name-counter (+ 1 name-counter)))))))

  (define (make-variable-namer var->name)
     (lambda ()
       (define name-counter 0)
       (lambda (vr)
         (begin0 (symbol "_" (var->name vr) "." name-counter)
           (set! name-counter (+ 1 name-counter))))))

  (define (make-simple-variable-namer)
    (make-variable-namer (const "")))

  (define (make-helpful-variable-namer)
    (make-variable-namer var-name))

  (define object? (disjoin pair? vector?))

  (define (make-reifier vm)
    (define seen-objects (make-eq-hashtable))
    (define variable-namer ((*variable-namer*)))
    (define (reify-object! obj)
      (hashtable-set! seen-objects obj obj)
      (cond
        [(pair? obj)
         (set-car! obj (reify! (car obj)))
         (set-cdr! obj (reify! (cdr obj)))]
        [(vector? obj)
         (let loop ([i 0])
           (when (< i (vector-length obj))
             (vector-set! obj i (reify! (vector-ref obj i)))
             (loop (+ i 1))))]
        [else
         (error 'reify-object!
                "Object must be either a pair or a vector")]))
    (define (reify! obj)
      (hashtable-ref-or-compute!
       seen-objects
       obj
       (lambda ()
         (cond
           [(and (var? obj)
                 (let ([cv (varmap-lookup obj vm)])
                   (and (not (eq? obj cv))
                        (var? obj)
                        cv)))
            => reify!]
           [(var? obj)
            (let* ([v* (varmap-lookup obj vm)])
              (cond
                [(var? v*)
                 (unless (eq? v* obj)
                   (error 'reify! "Reifier assumes objects are already seen"))
                 (let ([nm (variable-namer v*)])
                   (hashtable-set! seen-objects v* nm)
                   nm)]
                [(object? v*)
                 (hashtable-set! seen-objects obj v*)
                 (reify-object! v*)
                 v*]
                [else
                 v*]))]
           [(object? obj)
            (reify-object! obj)
            obj]
           [else obj]))))
    reify!)

  (define (reify-constraint con)
    (apply (constraint-reifier con) (constraint-operands con)))

  (define (reify-query qry st)
    (define reify! (make-reifier (varmap-copy (state-subst st))))

    (reify! (copy-object
             (cons qry
                   (lex-sort
                    (filter-map reify-constraint (state-facts st))))))))
