#!r6rs

(library (chrKanren prelude types)
  (export symbolo numbero stringo relationo
          typeo applyo callo)
  (import (rnrs)
          (chrKanren rule)
          (chrKanren goals)
          (chrKanren utils)
          (chrKanren syntax)
          (chrKanren prelude unification)
          (srfi :39 parameters))

  #|
  A type-constraint has four components:
  - The object (née `obj`), the thing being constrained
  - The type name (née `name`), the name of the type itself
  - The type predicate (née `pred`), a scheme procedure
  which recognizes (one-level) ground instances of the type
  - The type relation (née `prop`), a miniKanren relation
  which recognizes instances of the type, or #f, if the type
  is atomic.
  |#

  (define-constraint (typeo obj name _pred _prop)
    (list name obj))

  (define make-typeo
    (case-lambda
      [(name pred prop) (lambda (obj) (typeo obj name pred prop))]
      [(name pred) (make-typeo name pred #f)]))

  (define symbolo   (make-typeo 'sym symbol?))
  (define numbero   (make-typeo 'num number?))
  (define stringo   (make-typeo 'str string?))
  (define relationo (make-typeo 'rel procedure?))

  (define *atomic-type-predicates*
    (make-parameter (list symbol? number? string?)))

  (define (atomic-type-predicate? obj)
    (memq obj (*atomic-type-predicates*)))

  (define (callo rel . args)
    (applyo rel args))

  (define (applyo rel . arg-then-args)
    (let-values ([(rdc rac) (rdc+rac arg-then-args)])
      (conj (relationo rel)
            (%applyo rel (append rdc rac)))))

  (define-constraint (%applyo proc args)
    `(applyo ,proc ,args))

  (define-rules
    (forall (x y)
      (forget (%applyo x y))
      (ground list? y)
      (ground procedure? x)
      =>
      (apply x y))
    (forall (x y)
      (forget (== x y))
      (ground procedure? x)
      (ground procedure? y)
      (ground (negate eq?) x y)
      =>
      fail))

  (define-rules
    (forall (o n p)
      (forget (typeo o n p #f))
      (ground proccall p o))
    (forall (o n p pr)
      (forget (typeo o n p pr))
      (ground procedure? pr)
      (ground proccall p o)
      =>
      (callo pr o))
    (forall (o n p pr)
      (forget (typeo o n p pr))
      (ground (negate proccall) p o)
      =>
      fail)
    (forall (o n p pr)
      (typeo o n p pr)
      (forget (typeo o n p pr)))
    (forall (o n n^ p p^ pr pr^)
      (forget (typeo o n p pr))
      (forget (typeo o n^ p^ pr^))
      (ground (negate eq?) n n^)
      =>
      fail)
    (forall (o n p vs pr)
      (reifying vs)
      (forget (typeo o n p pr))
      (scheme (lambda (o vs)
                (not (find-subtree
                      (lambda (needle) (eq? o needle))
                      vs)))
              o
              vs))))
