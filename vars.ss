#!r6rs

(library (chrKanren vars)
  (export *var-counter*
          var var-name make-var var? var-idx
          var=?
          fresh
          younger+older-var)
  (import (rnrs) (chrKanren utils) (srfi :39))

  (define (assert-natural x)
    (if (and (integer? x) (not (negative? x)))
        x
        (assertion-violation '*var-counter*
                             "counter must be a natural number"
                             x)))

  (define *var-counter* (make-parameter 0 assert-natural))

  (define-record-type var
    (fields name idx)
    (protocol
     (lambda (new)
       (lambda (name)
         (*var-counter* (+ 1 (*var-counter*)))
         (new name (- (*var-counter*) 1))))))

  (define (var=? v1 v2)
    (and (var? v1) (var? v2) (= (var-idx v1) (var-idx v2))))

  (define-syntax-rule (fresh (var-name ...)
                        body body* ...)
    (let ([var-name (make-var 'var-name)]
          ...)
      body body* ...))

  (define (val-age vl)
    (if (var? vl)
        (var-idx vl)
        +inf.0))

  (define (younger+older-var v1 v2)
    (assert (not (var=? v1 v2)))
    (if (< (val-age v1) (val-age v2))
        (values v1 v2)
        (values v2 v1))))
