#!r6rs

(library (chrKanren prelude grammar)
  (export)
  (import (rnrs)
          (chrKanren base)
          (chrKanren prelude types)
          (chrKanren utils)
          (srfi :39 parameters))

  (define-constraint (matches-grammaro gram obj)
    `(matches-grammaro ,gram ,obj))

  (define-rules
    (forall (x) (matches-grammaro #t x) <=> succeed)
    (forall (x) (matches-grammaro #f x) <=> fail)
    (forall (x n)
      (matches-grammaro `(call ,n) x)
      <=>
      (applyo n x))
    (forall (x y)
      (matches-grammaro `(lit ,x) y)
      <=>
      (== y x))
    (forall (l r y)
      (matches-grammaro `(or ,l ,r) y)
      <=>
      (disj (matches-grammaro l y) (matches-grammaro r y)))
    (forall (l r y)
      (matches-grammaro `(and ,l ,r) y)
      <=>
      (matches-grammaro l y)
      (matches-grammaro r y))
    (forall (l r a d)
      (matches-grammaro `(pairof ,l ,r) `(,a . ,d))
      <=>
      (matches-grammaro l a)
      (matches-grammaro r d))
    (forall (l r p)
      (matches-grammaro `(pairof ,l ,r) p)
      (ground (negate pair?) p)
      <=>
      fail)))
