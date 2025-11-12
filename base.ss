#!r6rs

(library (chrKanren base)
  (export define-relation
          fresh == conj disj
          conde succeed fail
          run run*
          =/= symbolo numbero)
  (import (rnrs)
          (chrKanren vars)
          (chrKanren utils)
          (chrKanren syntax)
          (chrKanren goals)
          (chrKanren interp)
          (chrKanren state)
          (chrKanren relation)
          (chrKanren rule)
          (chrKanren streams)
          (chrKanren prelude unification)
          (chrKanren prelude disunification)
          (chrKanren prelude types)))
