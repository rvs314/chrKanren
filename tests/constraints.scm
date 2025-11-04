#!r6rs

(import (rnrs)
        (chrKanren test)
        (chrKanren vars)
        (chrKanren state)
        (chrKanren subst)
        (chrKanren streams)
        (chrKanren interp)
        (chrKanren unify)
        (chrKanren goals)
        (chrKanren constraint))


(declare-functor (symbolo obj))

(define-test declaration-test
  (check (procedure? symbolo))

  (let* ([pst (symbolo 'foobar)]
         [con (posting-constraint pst)]
         [fnc (constraint-functor con)])
    (check (posting? pst))
    (check (constraint? con))
    (check (functor? fnc))
    (check (eq? (functor-name fnc) 'symbolo))
    (check (equal? (functor-arglist fnc) '(obj)))
    (check (equal? (constraint-operands con) '(foobar)))))
