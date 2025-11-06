#!r6rs

(import (rnrs)
        (chrKanren test)
        (chrKanren vars)
        (chrKanren state)
        (chrKanren streams)
        (chrKanren interp)
        (chrKanren unify)
        (chrKanren goals))


(declare-constraint (symbolo obj))

(define-test declaration-test
  (check (procedure? symbolo))

  (let* ([pst (symbolo 'foobar)]
         [con (posting-constraint pst)]
         [fnc (constraint-constructor con)])
    (check (posting? pst))
    (check (constraint? con))
    (check (eq? fnc symbolo))))
