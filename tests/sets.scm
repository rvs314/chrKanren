#!r6rs

(import (rnrs)
        (chrKanren utils)
        (chrKanren base)
        (chrKanren check)
        (chrKanren test)
        (chrKanren prelude sets)
        (only (srfi :1 lists) lset=))

(define-test constructors
  (check (set? set-nil))
  (check (set-null? set-nil))
  (check (set-null? (set)))
  (check (set-pair? (set 1)))
  (check (set-pair? (set-cons 1 2)))
  (check (set? (set (set))))

  (check (equal? (set-first (set 1 2 3))
                 1))
  (check (equal? (set-head (set 1 2 3))
                 '(1 2 3)))
  (check (equal? (set-rest (set 1 2 3))
                 (set 2 3)))
  (check (equal? (set-tail (set 1 2 3))
                 set-nil)))

(define-test type-constraints
  (check (equal?
          (run* (p) (seto p))
          '(((_.0) (set _.0)))))
  (check (equal?
          (run* (p) (seto p) (== p set-nil))
          '(((#(set))))))
  (check (equal?
          (run* (p) (seto p) (== p (set 1 2 3)))
          `(((,(set 1 2 3))))))
  (check (equal?
          (run* (p q) (seto p) (== p (set* 1 2 3 q)))
          `(((,(set* 1 2 3 '_.0) _.0) (set _.0)))))
  ;; NOTE: this is a proof that the `set*` constructor is
  ;; unsafe, as it doesn't put a set constraint on the set-rest
  (check (equal?
          (run* (p q) (== p (set* 1 2 3 q)))
          `(((,(set* 1 2 3 '_.0) _.0))))))

(define-test ino-tests
  (check (equal?
          (run* ()
            (ino 3 (set 2 4 9 3 1)))
          '((()))))

  (check (equal?
          (run* ()
            (ino 3 ∅))
          '()))

  (check (equal?
          (run* (q)
            (ino 3 q))
          `(((,(set-cons 3 '_.0)) (set _.0)))))

  (check (equal?
          (run* (q)
            (ino 3 q)
            (== q ∅))
          '()))

  (check (equal?
          (run* (q r)
            (ino 3 q)
            (== q (set-cons r ∅)))
          `(((,(set 3) 3)) ((,(set 3 3) 3)))))

  (check (equal?
          (run* (q r)
            (== q (set-cons r ∅))
            (ino 3 q)
            (ino 4 q))
          '()))

  (check (equal?
          (run* (q r)
            (ino 3 q)
            (ino 4 q)
            (== q (set-cons r ∅)))
          '()))


  (check (equal?
          (run* (q)
            (fresh (k)
              (== k (set-cons 1 q))
              (ino 1 k)))
          `(((_.0) (set _.0))
            ((,(set-cons 1 '_.0)) (set _.0)))))

  (check (equal?
          (run* (q)
            (ino q (set 1 1 1 1)))
          '(((1)) ((1)) ((1)) ((1))))))

(define-test test-unification
  (check (lset= equal?
          (run* (q)
            (== (set 1 2) (set 1 2)))
          '(((_.0)))))

  (check (lset= equal?
          (run* (q)
            (== (set 1 q) (set 1 q)))
          '(((1)) ((_.0)))))

  (check (lset= equal?
          (run* (p q r)
            (== (set p q r) (set p q r)))
          '(((_.0 _.0 _.0))
            ((_.0 _.0 _.1))
            ((_.0 _.1 _.0))
            ((_.0 _.1 _.1))
            ((_.0 _.1 _.2)))))

  (check
   (lset= equal?
    (run* (X Y R S)
      (== (set-cons X Y) (set-cons R S)))
    `(((_.0 _.1 _.0 _.1) (set _.1))
      ((_.0 _.1 _.0 ,(set* '_.0 '_.1)) (set _.1))
      ((_.0 ,(set* '_.0 '_.1) _.0 _.1) (set _.1))
      ((_.0 ,(set* '_.1 '_.2) _.1 ,(set* '_.0 '_.2)) (set _.2))))))

(define-test occurs-check-sets
  (check
   (equal? ; "Subterm fails occurs-check"
    (run* (p)
      (== p (set p)))
    '()))
  (check
   (equal? ; "\"recursive\" set tails are equivalent to fresh variables"
    (run* (p)
      (== p (set* 1 p)))
    `((,(set* 1 '_.0) (set _.0)))))
  (check
   (equal? ; "Shorter race"
    (run* (a b)
      (== b (set-cons 6 a))
      (== a (set-cons 5 b)))
    `(((,(set* 5 6 '_.0) ,(set* 5 6 '_.0))
       (set _.0)))))
  (check ; "Relay race"
   (equal? (run* (p q r)
             (== p (set* 1 q))
             (== q (set* 2 r))
             (== r (set* 3 p)))
           `(((,(set* 1 2 3 '_.0) ,(set* 1 2 3 '_.0) ,(set* 1 2 3 '_.0))
              (set _.0))))))
