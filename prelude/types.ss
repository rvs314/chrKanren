#!r6rs

(library (chrKanren prelude types)
  (export symbolo numbero stringo relationo
          typeo applyo callo)
  (import (rnrs)
          (chrKanren rule)
          (chrKanren goals)
          (chrKanren utils)
          (chrKanren syntax)
          (chrKanren prelude unification))

  #|
  A type-constraint has four components:
  - The object (née `obj`), the thing being constrained
  - The type name (née `name`), the name of the type itself
  - The type predicate (née `pred`), a scheme procedure
    which recognizes (one-level) ground instances of the type
  - The type relation (née `prop`), a miniKanren relation
    which recognizes instances of the type, or #f
  |#

  (define-constraint (typeo obj name _pred _prop)
    (list name obj))

  (define (symbolo obj)   (typeo obj 'sym symbol?    #f))
  (define (numbero obj)   (typeo obj 'num number?    #f))
  (define (stringo obj)   (typeo obj 'str string?    #f))
  (define (relationo obj) (typeo obj 'rel procedure? #f))

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
      (%applyo x y)
      (ground list? y)
      (ground procedure? x)
      <=>
      (apply x y))
    (forall (x y)
      (== x y)
      (ground procedure? x)
      (ground procedure? y)
      (ground (negate eq?) x y)
      <=>
      fail))

  (define-rules
    (forall (o n p)
      (typeo o n p #f)
      (ground proccall p o)
      <=>
      succeed)
    (forall (o n p pr)
      (typeo o n p pr)
      (ground procedure? pr)
      (ground proccall p o)
      <=>
      (callo pr o))
    (forall (o n p pr)
      (typeo o n p pr)
      (ground (negate proccall) p o)
      <=>
      fail)
    (forall (o n p pr)
      (typeo o n p pr)
      (typeo o n p pr)
      <=>
      (typeo o n p pr))
    (forall (o n n^ p p^ pr pr^)
      (typeo o n p pr)
      (typeo o n^ p^ pr^)
      (ground (negate eq?) n n^)
      <=>
      fail)
    (forall (o n p vs pr)
      (reifying vs)
      (typeo o n p pr)
      (scheme (lambda (o vs)
                (not (find-subtree
                      (lambda (needle) (eq? o needle))
                      vs)))
              o
              vs)
      <=>
      (reifying vs))))
