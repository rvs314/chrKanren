#!r6rs

(import (rnrs)
        (chrKanren test)
        (chrKanren utils)
        (chrKanren base)
        (chrKanren syntax)
        (chrKanren vars)
        (chrKanren state)
        (chrKanren reifier)
        (chrKanren streams)
        (chrKanren interp)
        (chrKanren unify)
        (chrKanren rule)
        (chrKanren varmap)
        (chrKanren prelude disunification)
        (chrKanren goals)
        (prefix (srfi :41 streams) s:)
        (only (srfi :1 lists) lset=))


(define-relation (loopy x)
  (loopy x))

(define gl (fresh (q) (disj (== q 1) (loopy q))))
(define strm (start empty-state gl))
