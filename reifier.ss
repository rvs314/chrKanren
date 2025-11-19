#!r6rs

(library (chrKanren reifier)
  (export reify reify-query)
  (import (rnrs)
          (only (srfi :1 lists) list-index delete-duplicates filter-map)
          (chrKanren vars)
          (chrKanren compare)
          (chrKanren goals)
          (chrKanren vars)
          (chrKanren utils)
          (chrKanren state))

  (define (reify obj)
    (define var-counter 0)
    (define/memoized (name vr)
      (begin0 (symbol "_." var-counter)
        (set! var-counter (+ 1 var-counter))))
    (let loop ([obj obj])
      (cond
        [(pair? obj) (cons (loop (car obj)) (loop (cdr obj)))]
        [(var? obj) (name obj)]
        [else obj])))

  (define (reify-query qry st)
    (define (reify-constraint con)
      (apply (constraint-reifier con) (constraint-operands con)))

    (let-values ([(vr cn) (query st qry)])
      (define vs (single-out vr))
      (define c0 (filter-map reify-constraint cn))
      (if (null? c0)
          (reify vs)
          (let* ([c1 (map (lambda (d1) (cons (car d1) (lex-sort (cdr d1))))
                          (group-by car+cdr c0))]
                 [c2 (reify (cons vs c1))])
            c2)))))
