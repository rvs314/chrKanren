#!r6rs

(library (chrKanren prelude)
  (export == =/= symbolo numbero ground debug)
  (import (rnrs)
          (chrKanren vars)
          (chrKanren utils)
          (chrKanren syntax)
          (chrKanren goals)
          (chrKanren relation)
          (chrKanren rule))

  (define (debug . xs)
    (apply scheme (lambda os (puts os) #t) xs))

  (define (ground pred . os)
    (define (any-vars? . os) (exists var? os))
    (apply scheme (conjoin (negate any-vars?) pred) os))

  (define-constraint (symbolo obj) `(symbolo ,obj))
  (define-constraint (numbero obj) `(numbero ,obj))
  (define-constraint (== left right) (error '== "Should not reify"))
  (define-constraint (=/= left right) (error '=/= "Should not reify"))
  (define-constraint (=/=* alist)
    (if (= 1 (length alist))
        `(=/= ,(caar alist) ,(cdar alist))
        `(=/= ,(map car alist) ,(map cdr alist))))

  (define atom?
    (disjoin null? number? string? char?
             boolean? symbol? bytevector?))

  (define-rules
    (forall (x)
      (== x x))
    (forall (x y)
      (== x y)
      (ground atom? x)
      (ground (negate equal?) x y)
      <=>
      fail)
    (forall (x y)
      (== x y)
      (ground atom? y)
      (ground (negate equal?) x y)
      <=>
      fail)
    (forall (a b c d)
      (== (cons a b) (cons c d))
      <=>
      (== a c)
      (== b d))
    (forall (x y)
      (== x y)
      (scheme var? x)
      <=>
      (<- x y))
    (forall (x y)
      (== x y)
      (scheme var? y)
      <=>
      (<- y x)))

  (define-rules
    (forall (x) (symbolo x) (ground symbol? x))
    (forall (x) (symbolo x) (ground (negate symbol?) x) <=> fail)
    (forall (x) (numbero x) (ground number? x))
    (forall (x) (numbero x) (ground (negate number?) x) <=> fail)
    (forall (x) (symbolo x) (numbero x) <=> fail))

  (define-rules
    (forall (x y)
      (=/= x y)
      <=>
      (=/=* (list (cons x y))))
    (forall ()
      (=/=* (list))
      <=>
      fail)
    (forall (l ls)
      (=/=* (cons (cons l l) ls))
      <=>
      (=/=* ls))
    (forall (l r rs)
      (=/=* (cons (cons l r) rs))
      (ground atom? l)
      (ground (negate equal?) l r)
      <=>
      succeed)
    (forall (l r rs)
      (=/=* (cons (cons l r) rs))
      (ground atom? r)
      (ground (negate equal?) l r)
      <=>
      succeed)
    (forall (l r rs)
      (=/=* (cons (cons l r) rs))
      (numbero l)
      (symbolo r)
      <=>
      (numbero l)
      (symbolo r))
    (forall (l r rs)
      (=/=* (cons (cons l r) rs))
      (numbero l)
      (symbolo r)
      <=>
      (numbero l)
      (symbolo r))
    (forall (ll lr rl rr rs)
      (=/=* (cons (cons (cons ll lr) (cons rl rr)) rs))
      <=>
      (=/=* (cons* (cons ll rl) (cons lr rr) rs)))))
