#!r6rs

(library (chrKanren reifier)
  (export reify)
  (import (rnrs) (chrKanren vars) (chrKanren utils))

  (define (reify obj)
    (define var-counter 0)
    (define/memoized (name _)
      (begin0 (string->symbol (string-append "_." (number->string var-counter)))
        (set! var-counter (+ 1 var-counter))))
    (let loop ([obj obj])
              (cond
                [(pair? obj) (cons (loop (car obj)) (loop (cdr obj)))]
                [(var? obj) (name obj)]
                [else obj]))))
