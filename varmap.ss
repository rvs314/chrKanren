#!r6rs

(library (chrKanren varmap)
  (export empty-varmap
          varmap?
          varmap-lookup
          varmap-extend
          varmap-extend-all
          varmap-update
          varmap->alist)
  (import (rnrs) (chrKanren check) (chrKanren vars) (chrKanren utils))

  (define-record-type varmap (fields contents))

  (define empty-varmap (make-varmap '()))

  (define (varmap-lookup key vmap)
    (check (varmap? vmap))
    (let ([rs (assq key (varmap-contents vmap))])
      (if rs
          (cdr rs)
          key)))

  (define (varmap-extend key value vmap)
    (check (varmap? vmap))
    (make-varmap (cons (cons key value) (varmap-contents vmap))))

  (define (varmap-extend-all alist vmap)
    (check (varmap? vmap))
    (fold-left (lambda (acc k.v) (varmap-extend (car k.v) (cdr k.v) acc)) vmap alist))

  (define (varmap-update key fn vmap)
    (check (varmap? vmap))
    (varmap-extend key (fn (varmap-lookup key vmap)) vmap))

  (define (varmap->alist val)
    (check (varmap? val))
    (varmap-contents val)))
