#!r6rs

(import (rnrs) (chrKanren vars) (chrKanren subst) (srfi :64) (srfi :27))

(test-begin "subst")

(define a (make-var 'a))
(define b (make-var 'b))

(test-group "Simple extend/lookup"
  (test-eq "Empty lookup hits top"
      a (lookup a empty-subst))
  (test-assert "Reasonable extend call"
    (extend a 3 '()))
  (test-eqv "Reasonable lookup call"
    3 (lookup a (extend a 3 '())))
  (test-eqv "Shadowed lookup call"
    3 (lookup a (extend a 3 (extend a 2 '()))))
  (test-eq "Var lookup"
    b (lookup a (extend a b (extend a 2 '())))))

(test-group "Occurs-check"
  (test-assert "Circular extend fails"
    (not (extend a a '())))
  (test-assert "Occurs-check recurs"
    (not (extend a `(foobar a ,a) '())))
  (test-assert "Mutually recursive vars"
    (not (extend a (cons 1 b) (extend b (cons 2 a) '())))))

(test-group "walk(*)"
  (test-equal "Trivial walk*"
    `(a: ,a b: ,b)
    (walk*  `(a: ,a b: ,b) '()))

  (test-equal "Compound walk*"
    '(a: 3 b: 4)
    (walk* `(a: ,a b: ,b) (extend a 3 (extend b 4 empty-subst))))

  (test-equal "Partial walk*"
    `(a: 3 b: ,b)
    (walk* `(a: ,a b: ,b) (extend a 3 empty-subst)))

  (test-equal "out-of-order extending for walk"
    3
    (walk b (extend a 3 (extend b a empty-subst)))))

(test-end "subst")
