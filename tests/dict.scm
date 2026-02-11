#!r6rs

(import (rnrs)
        (only (srfi :1 lists) lset<=)
        (chrKanren utils)
        (chrKanren base)
        (only (chrKanren prelude lists) for-allo))

(define (dict-cons k v dct) `#(dict k v ,dct))

(define dict-nil `#(dict))

(define (dict? obj)
  (and (vector? obj)
       (memv (vector-length obj) '(1 3))
       (eq? (vector-ref obj 0) 'dict)))
