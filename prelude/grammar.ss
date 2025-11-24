#!r6rs

(library (chrKanren prelude grammar)
  (export)
  (import (rnrs) (chrKanren base) (chrKanren utils) (srfi :39 parameters))

  (define *nonterminals* (make-parameter (make-eq-hashtable)))

  (define-constraint (matches-grammaro gram obj)
    `(matches-grammaro ,gram ,obj))

  (define-syntax-rule (define-nonterminal name defn)
    (define frombnicus
      (begin
        (hashtable-set! (*nonterminals*) 'name (quote defn)))))

  (define-rules
    (forall (x) (matches-grammaro #t x) <=> succeed)
    (forall (x) (matches-grammaro #f x) <=> fail)
    (forall (x n)
      (matches-grammaro n x)
      (ground symbol? n)
      <=>
      (matches-grammaro (hashtable-ref (*nonterminals*) n #f) x))
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
      fail)
    (forall (x y)
      (matches-grammaro x y)
      (ground boolean? x)
      <=>
      (matches-grammaro (not x) y))
    (forall (x y)
      (matches-grammaro x y)
      (ground symbol? x)
      <=>
      (matches-grammaro `(not ,(hashtable-ref (*nonterminals*) x #f)) y))
    (forall (x y)
      (matches-grammaro `(not (lit ,x)) y)
      <=>
      (=/= x y))
    (forall (l r y)
      (matches-grammaro `(not (or ,l ,r)) y)
      <=>
      (matches-grammaro `(and (not ,l) (not ,r))) y)
    (forall (l r y)
      (matches-grammaro `(not (and ,l ,r)) y)
      <=>
      (matches-grammaro `(or (not ,l) (not ,r))) y)
    (forall (l r y)
      (matches-grammaro `(not (pairof ,l ,r)) y)
      (ground (negate pair?) y)
      <=>
      succeed)
    (forall (l r x y)
      (matches-grammaro `(not (pairof ,l ,r)) `(,x . ,y))
      <=>
      (matches-grammaro `(not ,l) x)
      (matches-grammaro `(not ,r) y))
    (forall (l y)
      (matches-grammaro `(not (not ,l)) y)
      <=>
      (matches-grammaro l y))))
