#!r6rs

(import (rnrs))

(define types (list null? pair? string? number? symbol?))

(define (type-of obj)
  (find (lambda (t) (t obj)) types))

#;
(define-constraint (has-typeo obj ty))

#;
(define-rules
  ((has-typeo x p)
   (has-typeo x q)
   (guard (not (var=? p q)))
   =>
   fail)
  ((has-typeo x p) (guard (p (ground x))) => succeed)
  ((has-typeo x p) (guard (not (p (ground x)))) => fail))

;; Expands into...

(define (constraint-assert constraint args)
  (lambda (st)
    (check-constraints (add-constraint st (cons constraint args)))))

(define (constraint-query constraint args)
  (lambda (st)
    (bind
     (state-constraints st)
     (lambda (con)
       (define-values (u a) (unify-trivially (cons con args) c1 st))
       (if u
           (unit (state-with-assoc st u))
           mzero)))))

(define *constraint-handler* (make-parameter constraint-assert))

(define *handling-rules* (make-parameter '()))

(define (has-typeo x y)
  ((*constraint-handler*) has-typeo (list x y)))
