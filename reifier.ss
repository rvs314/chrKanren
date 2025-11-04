#!r6rs

(library (chrKanren reifier)
  (export reify reify-query)
  (import (rnrs) (chrKanren vars) (chrKanren utils) (chrKanren state))

  (define (reify obj)
    (define var-counter 0)
    (define/memoized (name _)
      (begin0 (string->symbol (string-append "_." (number->string var-counter)))
        (set! var-counter (+ 1 var-counter))))
    (let loop ([obj obj])
      (cond
        [(pair? obj) (cons (loop (car obj)) (loop (cdr obj)))]
        [(var? obj) (name obj)]
        [else obj])))

  (define (reify-query qry st)
    (let-values ([(vr cn) (query st qry)])
      (if (null? cn)
          (reify vr)
          (reify `(,vr where ,@cn))))))
