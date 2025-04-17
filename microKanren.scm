#!r6rs

(library (chrKanren microKanren)
  (export var var? var=? var-min var-max var>=?
          state? state-assoc state-constraints state-count
          state-with-assoc state-with-constraints state-with-counter
          allocate-variable add-constraint
          empty-state walk walk* check-constraints constraint->goal
          == unify unify-trivially
          unit mzero bind mplus
          succeed fail conj disj
          call/fresh)

  (import (rnrs))

  (define (var c) (vector c))
  (define (var? x) (vector? x))
  (define (var-idx v) (vector-ref v 0))
  (define (var=? x1 x2) (= (var-idx x1) (var-idx x2)))
  (define (var-min x y) (var (min (var-idx x) (var-idx y))))
  (define (var-max x y) (var (max (var-idx x) (var-idx y))))
  (define (var>=? v1 v2) (>= (var-idx v1) (var-idx v2)))

  (define-record-type state
    (fields assoc constraints count))

  (define (state-with-assoc state assoc)
    (make-state assoc (state-constraints state) (state-count state)))

  (define (state-with-constraints state constraints)
    (make-state (state-assoc state) constraints (state-count state)))

  (define (state-with-counter state counter)
    (make-state (state-assoc state) (state-constraints state) counter))

  (define (allocate-variable state)
    (values (var (state-count state))
            (state-with-counter state (+ 1 (state-count state)))))

  (define (add-constraint state new-constraint)
    (state-with-constraints state (cons new-constraint (state-constraints state))))

  (define empty-state (make-state '() '() 0))

  (define (walk u s)
    (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
      (if pr (walk (cdr pr) s) u)))

  (define (walk* v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) v)
        ((pair? v) (cons (walk* (car v) s)
                         (walk* (cdr v) s)))
        (else  v))))

  (define (ext-s x v s) `((,x . ,v) . ,s))

  (define (check-constraints st)
    (fold-left (lambda (a e) (bind a (constraint->goal e)))
               (unit (state-with-constraints st '()))
               (state-constraints st)))

  (define (constraint->goal con)
    (apply (car con) (cdr con)))

  (define (== u v)
    (lambda (st)
      (let-values (((s added) (unify u v (state-assoc st))))
        (if s
            (check-constraints (state-with-assoc st s))
            mzero))))

  (define (unit state) (cons state mzero))
  (define mzero '())

  (define (succeed st) (unit st))
  (define (fail st) mzero)

  (define (unify u v s)
    (let ((u (walk u s)) (v (walk v s)))
      (cond
        ((and (var? u) (var? v) (var=? u v)) s)
        ((and (var? u) (var? v))
         (let ((mn (var-min u v))
               (mx (var-max u v)))
           (values (ext-s mx mn s) `((,mx . ,mn)))))
        ((var? u) (values (ext-s u v s) `((,u . ,v))))
        ((var? v) (values (ext-s v u s) `((,v . ,u))))
        ((and (pair? u) (pair? v))
         (let-values (((s added) (unify (car u) (car v) s)))
           (if s
               (let-values (((s2 added2) (unify (cdr u) (cdr v) s)))
                 (values s (append added added2)))
               (values #f #f))))
        (else
         (if (eqv? u v)
             (values s '())
             (values #f #f))))))

  (define (unify-trivially u v state)
    (let-values (((state^ added) (unify u v (state-assoc state))))
      (assert (for-all pair? added))
      (and (for-all (lambda (as) (var>=? (car as) (var (state-count state)))) added)
           (values state^ added))))

  (define (call/fresh f)
    (lambda (st)
      (let-values (((vr st^) (allocate-variable st)))
        ((f vr) st^))))

  (define (disj g1 g2) (lambda (state) (mplus (g1 state) (g2 state))))
  (define (conj g1 g2) (lambda (state) (bind (g1 state) g2)))

  (define (mplus $1 $2)
    (cond
      ((null? $1) $2)
      ((procedure? $1) (lambda () (mplus $2 ($1))))
      ((pair? $1) (cons (car $1) (mplus (cdr $1) $2)))
      (else (error 'mplus "Invalid Stream" $1))))

  (define (bind $ g)
    (cond
      ((null? $) mzero)
      ((procedure? $) (lambda () (bind ($) g)))
      ((pair? $) (mplus (g (car $)) (bind (cdr $) g)))
      (else (error 'mplus "Invalid Stream" $)))))
