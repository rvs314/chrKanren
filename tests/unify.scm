#!r6rs

(import (rnrs)
        (chrKanren utils)
        (chrKanren check)
        (chrKanren vars) (chrKanren unify)
        (chrKanren subst) (chrKanren state)
        (chrKanren goals)
        (chrKanren streams) (chrKanren test)
        (chrKanren interp)
        (srfi :39 parameters))


(define-test smoke-tests
  (check (equal?
          (list empty-state)
          (take-finite (unify 1 1 empty-state))))
  (check (equal?
          (list)
          (take-finite (unify 1 2 empty-state)))))

(define-test var-bindings
  (let* ([st  empty-state]
         [p   (make-var 'p)]
         [q   (make-var 'q)]
         [st* (unify p q st)]
         [_   (check (solution? st*))]
         [st0 (solution-first st*)])
    (check (not (var=? p q)))
    (check (var=? p (state-lookup p st)))
    (check (var=? q (state-lookup q st)))
    (check (var=? q (state-lookup p st0)))
    (check (var=? q (state-lookup q st0)))))

(define-test occurs-check
  (let* ([p   (make-var 'p)]
         [st* (unify p (cons 1 p) empty-state)])
    (check (empty? st*))))

(define-test triangular-substitution
  (let* ([st  empty-state]
         [p   (make-var 'p)]
         [q   (make-var 'q)]
         [r   (make-var 'r)]
         [st* (take-finite (unify (list p q) (list q r) st))]
         [_   (check (pair? st*))]
         [st0 (car st*)])
    (check (var=? p (state-lookup p st)))
    (check (var=? q (state-lookup q st)))
    (check (var=? r (state-lookup r st)))
    (check (var=? q (state-lookup p st0)))
    (check (var=? r (state-lookup q st0)))
    (check (var=? r (state-lookup r st0)))))

(define-test dual-pair
  (let* ([p   (make-var 'p)]
         [q   (make-var 'q)]
         [r   (make-var 'r)]
         [st* (take-finite (unify `(,p . 1) `(2 . ,q) empty-state))]
         [_   (check (pair? st*))]
         [st0 (car st*)])
    (check (equal? 2 (state-lookup p st0)))
    (check (equal? 1 (state-lookup q st0)))))
