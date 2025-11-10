#!r6rs

(library (chrKanren vars)
  (export *var-counter*
          var var-name make-var var? var-idx
          fresh var<=?)

  (import (rnrs)
          (chrKanren utils)
          (chrKanren goals)
          (srfi :39 parameters))

  (define (assert-natural x)
    (if (and (integer? x) (not (negative? x)))
        x
        (assertion-violation '*var-counter*
                             "counter must be a natural number"
                             x)))


  (define *var-counter* (make-parameter 0 assert-natural))

  (define-syntax fresh
    (syntax-rules ()
      [(fresh (var-name ...) body)
       (let ([var-name (make-var 'var-name)]
             ...)
         body)]
      [(fresh (var-name ...) body body* ...)
       (fresh (var-name ...) (conj body body* ...))]))

  (define-record-type var
    (fields name idx)
    (protocol
     (lambda (new)
       (lambda (name)
         (*var-counter* (+ 1 (*var-counter*)))
         (new name (- (*var-counter*) 1))))))

  (define var<=? (on <= var-idx)))
