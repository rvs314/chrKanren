#!r6rs

(library (chrKanren base)
  (export define-relation fresh == conj disj conde succeed fail run run*)
  (import (rnrs)
          (chrKanren vars)
          (chrKanren utils)
          (chrKanren syntax)
          (chrKanren goals)
          (chrKanren relation)
          (chrKanren constraint))

  (declare-constraint (symbolo obj))
  (declare-constraint (numbero obj))
  (declare-constraint (== left right))

  (define (ground pred . os)
    (define (any-vars? . os) (exists var? os))
    (apply scheme (conjoin (negate any-vars?) pred) os))

  (define (is-var obj)
    (scheme var? obj))

  (define (print-here . xs)
    (apply scheme (lambda os (puts os) #t) xs))

  ;; TODO: Currently can't seem to get rules to trigger recursively:
  ;; they think they've seen the results already.
  ;; The reason this happens is because they have, pretty much:
  ;; it triggers the same rule twice and (seeing no change) stops the loop.
  ;; I will probably need to avoid doing a trivial loop of constrain -> propagate -> constrain
  ;; and do something smarter
  (define-constraint-handling-rules
    [forall (x) (== x x) => succeed]
    [forall (x y) (== x y) (ground number? x) (ground number? y) (ground (negate equal?) x y)
            => fail]
    [forall (a b c d) (== (cons a b) (cons c d)) => (== a c) (== b d)]
    [forall (x y) (== x y) (scheme var? x) => (<- x y)]
    [forall (x y) (== x y) (scheme var? y) => (<- y x)]))
