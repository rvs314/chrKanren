#!r6rs

(library (chrKanren varmap)
  (export empty-varmap
          varmap-lookup
          varmap-extend varmap-extend-all
          varmap-update)
  (import (rnrs) (chrKanren check) (chrKanren vars))

  (define empty-varmap '())

  (define (varmap-lookup key vmap)
    (let ([rs (assq key vmap)])
      (if rs
          (cdr rs)
          key)))

  (define (varmap-extend key value vmap)
    (cons (cons key value) vmap))

  (define (varmap-extend-all alist vmap)
    (fold-left (lambda (acc k.v) (varmap-extend (car k.v) (cdr k.v) acc)) vmap alist))

  (define (varmap-update key fn vmap)
    (varmap-extend key (fn (varmap-lookup key vmap)) vmap)))
