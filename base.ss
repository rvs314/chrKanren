#!r6rs

(library (chrKanren base)
  (export define-relation fresh == conj disj conde succeed fail run run*)
  (import (rnrs)
          (chrKanren vars)
          (chrKanren syntax)
          (chrKanren goals)
          (chrKanren relation)))
