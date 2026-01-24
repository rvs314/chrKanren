#!r6rs

(library (chrKanren prelude sets)
  (export set-nil ∅ set-null?
          set-cons set-pair? set-first set-rest
          set?
          set-head set-tail
          list->set set set* make-set
          set-nullo set-conso seto ino)
  (import (rnrs)
          (chrKanren vars)
          (chrKanren base)
          (chrKanren check)
          (chrKanren utils)
          (chrKanren rule)
          (chrKanren prelude lists)
          (chrKanren prelude unification)
          (chrKanren prelude types))

  (define set-nil '#(set))
  (define ∅ set-nil)

  (define (set-null? obj)
    (equal? obj set-nil))

  (define (set-pair? obj)
    (and (vector? obj)
         (= (vector-length obj) 3)
         (eq? (vector-ref obj 0) 'set)))

  (define set? (disjoin set-null? set-pair?))

  (define (set-first st)
    (check (set-pair? st))
    (vector-ref st 1))

  (define (set-rest st)
    (check (set-pair? st))
    (vector-ref st 2))

  (define (set-cons h st)
    (vector 'set h st))

  (define (set-head st)
    (if (set-pair? st)
        (cons (set-first st) (set-head (set-rest st)))
        '()))

  (define (set-tail st)
    (if (set-pair? st)
        (set-tail (set-rest st))
        st))

  (define (make-set head tail)
    (check (list? head) 'make-set)
    (fold-right set-cons tail head))

  (define (list->set lst) (make-set lst set-nil) )

  (define (set . xs) (list->set xs))

  (define (set* . xs)
    (cond
      [(null? xs)
       set-nil]
      [(and (pair? xs) (null? (cdr xs)))
       (car xs)]
      [else
       (set-cons (car xs) (apply set* (cdr xs)))]))

  (define-relation (set-conso first rest res)
    (== res `#(set ,first ,rest))
    (seto rest))

  (define-relation (set-nullo obj)
    (== obj '#(set)))

  (define (seto obj) (typeo obj 'set set? valid-seto))

  (define-relation (valid-seto obj)
    (conde
     [(set-nullo obj)]
     [(fresh (h r)
        (=== (set-cons h r) obj)
        (seto r))]))

  (define-constraint (ino _obj _st)
    (error 'ino "should not reify"))

  (define-rules
    (forall (r)
      (ino r set-nil) <=> fail)
    (forall (r s t)
      (ino r (set-cons s t))
      <=>
      (seto t)
      (disj (== r s) (ino r t)))
    (forall (t X)
      (ino t X)
      (scheme var? X)
      <=>
      (seto X)
      (fresh (N)
        (== X (set-cons t N)))))

  (define parallel-sets? (on eq? set-tail))

  (define-rules
    (forall (t) (== t set-nil) <=> (=== t set-nil))
    (forall (t) (== set-nil t) <=> (=== set-nil t))
    (forall (t s p)
      (== p (set-cons t s))
      (ground (negate set-pair?) p)
      <=>
      fail)
    (forall (t s p)
      (== (set-cons t s) p)
      (ground (negate set-pair?) p)
      <=>
      fail)
    (forall (t s t^ s^)
      (== (set-cons t s) (set-cons t^ s^))
      (scheme (negate eq?) (set-tail s) (set-tail s^))
      <=>
      (seto s)
      (seto s^)
      (conde
       [(== t t^)
        (disj (== s s^)
              (== (set-cons t s) s^)
              (== s (set-cons t^ s^)))]
       [(fresh (N)
          (seto N)
          (== s (set-cons t^ N))
          (== s^ (set-cons t N)))]))
    (forall (t s t^ s^)
      (== (set-cons t s) (set-cons t^ s^))
      (scheme eq? (set-tail s) (set-tail s^))
      <=>
      (parallel-sets-== (cons t (set-head s))
                        (cons t^ (set-head s^))
                        (set-tail s))))

  ;; Implements rule 10 of Dovier
  (define (parallel-sets-== ts ts^ X)
    (existso
     (lambda (zp)
       (let-values ([(t0 t1..m)              (car+cdr ts)]
                    [(t^j-1..0 t^j t^j+1..n) (apply values zp)])
         (conde
          [(== t0 t^j)
           (parallel-sets-==
            t1..m
            (append t^j-1..0 t^j+1..n)
            X)]
          [(== t0 t^j)
           (parallel-sets-==
            (cons t0 t1..m)
            (append t^j-1..0 t^j+1..n)
            X)]
          [(== t0 t^j)
           (parallel-sets-==
            t1..m
            (append t^j-1..0 (list t^j) t^j+1..n)
            X)]
          [(fresh (N)
             (seto N)
             (set-conso t0 N X)
             (parallel-sets-==
              t1..m
              (append t^j-1..0 (list t^j) t^j+1..n)
              N))])))
     (zippers ts^)))

  (define-rules
    (forall (x rs z)
      (occurs-checko x (cons set-nil rs) z)
      <=>
      (occurs-checko x rs z))
    (forall (x t rs z)
      (occurs-checko x (cons (set-cons x t) rs) z)
      <=>
      fail)
    (forall (x rs z)
      (occurs-checko x rs z)
      (ground set-pair? z)
      (scheme (lambda (x z) (eq? x (set-tail z))) x z)
      <=>
      (fresh (N)
        (seto N)
        (occurs-checko x (set-head z) (make-set (set-head z) N))))
    (forall (x h t rs z)
      (occurs-checko x (cons (set-cons h t) rs) z)
      (scheme (negate set-pair?) z)
      (scheme (negate eq?) x h)
      (scheme (negate eq?) x t)
      <=>
      (occurs-checko x (cons* h t rs) z))
    (forall (x h t rs z)
      (occurs-checko x (cons (set-cons h t) rs) z)
      (ground set-pair? z)
      (scheme (lambda (x z) ((negate eq?) x (set-tail z))) x z)
      (scheme (negate eq?) x h)
      (scheme (negate eq?) x t)
      <=>
      (occurs-checko x (cons* h t rs) z))))
