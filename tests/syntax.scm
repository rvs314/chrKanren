#!r6rs

(import (rnrs)
        (chrKanren syntax)
        (chrKanren relation)
        (chrKanren goals)
        (chrKanren vars)
        (chrKanren interp)
        (chrKanren state)
        (chrKanren streams)
        (chrKanren check)
        (chrKanren test)
        (chrKanren utils)
        (only (srfi :1 lists) lset=)
        (srfi :39 parameters))


(define-relation (sample-relation p q r)
  (== (list p q r) '(n y c)))

(define-test test-simple-relation
  (check (procedure? sample-relation))
  (let* ([delayobj (sample-relation 1 2 3)]
         [_ (check (delay? delayobj))]
         [callobj ((delay-cont delayobj))]
         [_ (check (call? callobj))]
         [_ (check (equal? (call-arguments callobj) '(1 2 3)))]
         [relobj (call-target callobj)]
         [_ (check (relation? relobj))]
         [_ (check (eq? (relation-name relobj) 'sample-relation))]
         [_ (check (equal? (relation-args relobj) '(p q r)))]
         [_ (check (procedure? (relation-builder relobj)))]
         [body ((relation-builder relobj) 'a 'b 'c)]
         [_ (check (goal=? body (== (list 'a 'b 'c) '(n y c))))])
    'pass))

(define example-fresh
  (parameterize ([*var-counter* 0])
    (fresh (p q)
      (== p 1)
      (== q 2))))

(define-test test-fresh
  (parameterize ([*var-counter* 0])
    (let* ([_ (check (conjunction? example-fresh))]
           [left (conjunction-left example-fresh)]
           [right (conjunction-right example-fresh)]
           [_ (check (unification? left))]
           [_ (check (unification? right))]
           [l-l (unification-lhs left)]
           [l-r (unification-rhs left)]
           [r-l (unification-lhs right)]
           [r-r (unification-rhs right)]
           [_ (check (var? l-l))]
           [_ (check (var? r-l))]
           [_ (lset= = (map var-idx (list l-l r-l))
                       (list 0 1))]
           [_ (check (eqv? l-r 2))]
           [_ (check (eqv? r-r 1))])
      'passed)))

(define-test test-simple-conde
  (check (goal=?
          (conde
           [(== 'a 'b) (== 'c 'd)]
           [(== 'e 'f) (== 'g 'h)]
           [(== 'i 'j) (== 'k 'l)])
          (disj
           (conj (== 'a 'b) (== 'c 'd))
           (conj (== 'e 'f) (== 'g 'h))
           (conj (== 'i 'j) (== 'k 'l))))))
