#!r6rs

(import (rnrs)
        (chrKanren test)
        (chrKanren base)
        (chrKanren vars)
        (chrKanren state)
        (chrKanren streams)
        (chrKanren rule)
        (chrKanren utils)
        (chrKanren interp)
        (chrKanren unify)
        (chrKanren goals))


(define-constraint (symbolo obj)
  `(symbolo ,obj))

(define-test declaration-test
  (check (procedure? symbolo))

  (let* ([pst (symbolo 'foobar)]
         [con (posting-constraint pst)]
         [fnc (constraint-constructor con)])
    (check (posting? pst))
    (check (constraint? con))
    (check (eq? fnc symbolo))))

(define-rules
  [forall (obj) (symbolo obj) (ground symbol? obj)]
  [forall (obj) (symbolo obj) (ground (negate symbol?) obj) <=> fail])

(define-test symbolo-test
  (check (equal? (run* (p) (symbolo p))
                 '((_.0 (symbolo (_.0))))))
  (check (equal? (run* () (symbolo 'foo))
                 '(())))
  (check (equal? (run* () (symbolo 3))
                 '())))

(define-constraint (numbero obj)
  `(numbero ,obj))

(define-rules
  [forall (obj) (symbolo obj) (numbero obj) <=> fail]
  [forall (obj) (numbero obj) (ground number? obj) <=> succeed]
  [forall (obj) (numbero obj) (ground (negate number?) obj) <=> fail])

(define-test numbero-test
  (check (equal? (run* (p) (numbero p))
                 '((_.0 (numbero (_.0))))))
  (check (equal? (run* () (numbero 3))
                 '(())))
  (check (equal? (run* () (numbero 'foo))
                 '()))
  (check (equal? (run* (p) (numbero p) (symbolo p))
                 '())))
