#!r6rs

(import (rnrs)
        (chrKanren utils)
        (chrKanren base)
        (chrKanren check)
        (chrKanren test)
        (chrKanren interp)
        (chrKanren state)
        (chrKanren rule)
        (chrKanren prelude unification)
        (only (srfi :1 lists) lset=))

#|
Boxes are just single slots which do not follow the occurs-check.
|#

(define (box obj) `#(box ,obj))

(define (box? obj) (and (vector? obj)
                        (= 2 (vector-length obj))
                        (eq? (vector-ref obj 0) 'box)))

(define (unbox obj)
  (check (box? obj))
  (vector-ref obj 1))

(define (box-fill! obj val)
  (vector-set! obj 1 val))

(define-rules
  (forall (x y)
    (== (box x) (box y))
    <=>
    (== x y))
  (forall (x y)
    (== (box x) y)
    (ground (negate box?) y)
    <=>
    fail)
  (forall (x y)
    (== x (box y))
    (ground (negate box?) x)
    <=>
    fail)

  ;; Ignore all occurs-check tests
  (forall (x y rs z)
    (occurs-checko x (cons (box y) rs) z)
    <=>
    (occurs-checko x rs z)))

(define-test simple-unifications
  (check (equal? (run* () (== (box 2) (box 3)))
                 '()))
  (check (equal? (run* () (== (box 3) (box 3)))
                 '((()))))
  (check (equal? (run* (p) (== p (box 3)))
                 `(((,(box 3))))))
  (check (equal? (run* (p q) (== (box p) (box q)))
                 `(((_.0 _.0)))))
  (check (equal? (run* (p) (== (box p) 3))
                 '())))



(define-test recursive-object
  (define box-in-itself (box #f))
  (define box-in-list-in-itself (box #f))

  (box-fill! box-in-itself box-in-itself)
  (box-fill! box-in-list-in-itself (list box-in-list-in-itself))

  (check (equal? (run* (p) (== p (box p)))
                 `(((,box-in-itself)))))

  ;; Nested structures
  (check (equal? (run* (p) (== p (box (box p))))
                 `(((,box-in-itself)))))

  (check (equal? (run* (p) (== p (box (list p))))
                 `(((,box-in-list-in-itself)))))

  (check (equal? (run* (p q r)
                   (== p (box q))
                   (== q (box r))
                   (== r (box p)))
                 `(((,box-in-itself ,box-in-itself ,box-in-itself))))))
