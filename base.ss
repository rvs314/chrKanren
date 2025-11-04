#!r6rs

(library (chrKanren base)
  (export define-relation fresh == conj disj conde succeed fail run run*)
  (import (rnrs)
          (chrKanren vars)
          (chrKanren syntax)
          (chrKanren goals)
          (chrKanren relation)
          (chrKanren constraint))

  (declare-constraint (== left right))
  (declare-constraint (pass))

  (define-constraint-handling-rules
    [forall (l) (== l l) => (pass)]
    [forall (p q) (== l r) (ground equal? l r) => (pass)]))
